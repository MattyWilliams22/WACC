package wacc.extensions

import scala.collection.mutable.ListBuffer

import wacc.backend._

object Peephole {

  type Transformation = (Instruction, List[Instruction]) => (List[Instruction], List[Instruction])

  val transformations: List[Transformation] = List(removeRedundantSubAdd, removeRedundantCmp, removeRedundantMov, combineMovAdd, combineDoubleMov, removeRedundantStrLdr, removeRedundantLdr)

  def optimise(instr: Instruction, remaining: List[Instruction], transformations: List[Transformation] = transformations): (List[Instruction], List[Instruction]) = {
    transformations.foldLeft((List(instr), remaining)) { (current, transform) =>
      current match {
        case (Nil, newRemaining) => return (Nil, newRemaining)
        case _ => transform(current._1.head, current._2)
      }
    }
  }

  /* Removes instructions of the form
      add r0, r0, 0
      sub r0, r0, 0
  */
  def removeRedundantSubAdd(instr: Instruction, remaining: List[Instruction]): (List[Instruction], List[Instruction]) = {
    instr match {
      case AddInstr(dest, src, ImmVal(0), _) if dest == src => (Nil, remaining)
      case SubInstr(dest, src, ImmVal(0), _) if dest == src => (Nil, remaining)
      case _ => (List(instr), remaining)
    }
  }

  /* Removes instructions of the form
      cmp r0, 0
      cmp r0, 0
  */
  def removeRedundantCmp(instr: Instruction, remaining: List[Instruction]): (List[Instruction], List[Instruction]) = {
    instr match {
      case CmpInstr(dest, ImmVal(0)) => remaining match {
        case Nil => (List(instr), remaining)
        case head :: tail => head match {
          case CmpInstr(dest, ImmVal(0)) => (Nil, remaining)
          case _ => (List(instr), remaining)
        }
      }
      case _ => (List(instr), remaining)
    }
  }

  /* Removes instructions of the form
      mov r0, r0
  */
  def removeRedundantMov(instr: Instruction, remaining: List[Instruction]): (List[Instruction], List[Instruction]) = {
    instr match {
      case Mov(dest, src, _) if dest == src => (Nil, remaining)
      case _ => (List(instr), remaining)
    }
  }

  /* Combines instructions of the form
      mov r1, #1
      add r0, r0, r1
      =>
      add r0, r0, #1
  */
  def combineMovAdd(instr: Instruction, remaining: List[Instruction]): (List[Instruction], List[Instruction]) = {
    (instr, getNextInstruction(remaining)) match {
      case (Mov(dest1, ImmVal(value), _), Some(AddInstr(dest2, op1, op2, updateFlags))) if dest1 == op2 =>
        (List(AddInstr(dest2, op1, ImmVal(value), updateFlags)), dropInstructions(remaining, 1))
      case _ => (List(instr), remaining)
    }
    //(List(instr), remaining)
  }

  /* Combines instructions of the form
      mov r0, r1
      mov r2, r0
      =>
      mov r2, r1
  */
  def combineDoubleMov(instr: Instruction, remaining: List[Instruction]): (List[Instruction], List[Instruction]) = {
    // (instr, getNextInstruction(remaining)) match {
    //   case (Mov(dest1, src1, cond1), Some(Mov(dest2, src2, cond2))) if dest1 == src2 && cond1 == cond2 =>
    //     (List(Mov(dest2, src1, cond1)), dropInstructions(remaining, 1))
    //   case _ => (List(instr), remaining)
    // }
    (List(instr), remaining)
  }

  /* Remove redundant str ldr pairs
      str r0, [r1]
      ldr r0, [r1]
    OR
      ldr r0, [r1]
      str r0, [r1]
  */
  def removeRedundantStrLdr(instr: Instruction, remaining: List[Instruction]): (List[Instruction], List[Instruction]) = {
    (instr, getNextInstruction(remaining)) match {
      case (instr1@StrInstr(dest1, op1, _), Some(Ldr(dest2, op2))) if dest1 == dest2 && op1 == op2 =>
        (List(instr1), dropInstructions(remaining, 1))
      case (instr1@Ldr(dest1, op1), Some(StrInstr(dest2, op2, _))) if dest1 == dest2 && op1 == op2 =>
        (List(instr1), dropInstructions(remaining, 1))
      case _ => (List(instr), remaining)
    }
  }

  /* Removes the first ldr instruction when instructions are of the form
      ldr r0, [r1]
      ldr r0, [r2]
      =>
      ldr r0, [r2]
  */
  def removeRedundantLdr(instr: Instruction, remaining: List[Instruction]): (List[Instruction], List[Instruction]) = {
    (instr, getNextInstruction(remaining)) match {
      case (Ldr(dest1, op1), Some(Ldr(dest2, op2))) if dest1 == dest2 =>
        (List(Ldr(dest1, op2)), dropInstructions(remaining, 1))
      case _ => (List(instr), remaining)
    }
  }

  def getNextInstruction(remaining: List[Instruction]): Option[Instruction] = {
    remaining match {
      case Nil => None
      case Comment(_, _) :: tail => getNextInstruction(tail)
      case head :: _ => Some(head)
    }
  }

  def dropInstructions(remaining: List[Instruction], n: Int): List[Instruction] = {
    remaining match {
      case Nil => Nil
      case Comment(_, _) :: tail => dropInstructions(tail, n)
      case head :: tail if n > 0 => dropInstructions(tail, n - 1)
      case _ => remaining
    }
  }
}