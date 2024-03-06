package wacc.extensions

import scala.collection.mutable.ListBuffer

import wacc.backend._

object Peephole {

  type Transformation = (Instruction, List[Instruction]) => (List[Instruction], List[Instruction])

  val transformations: List[Transformation] = List(redundant_sub_add, redundant_cmp, redundant_move, combine_mov_add_sub, combine_mov_mov, redundant_str_ldr)

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
  def redundant_sub_add(instr: Instruction, remaining: List[Instruction]): (List[Instruction], List[Instruction]) = {
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
  def redundant_cmp(instr: Instruction, remaining: List[Instruction]): (List[Instruction], List[Instruction]) = {
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
  def redundant_move(instr: Instruction, remaining: List[Instruction]): (List[Instruction], List[Instruction]) = {
    instr match {
      case Mov(dest, src, _) if dest == src => (Nil, remaining)
      case _ => (List(instr), remaining)
    }
  }

  /* Combines instructions of the form
      mov r0, r1
      add r0, r0, #1
      =>
      add r0, r1, #1
    OR
      mov r0, #1
      add r0, r0, r1
      =>
      add r0, r1, #1
  */
  def combine_mov_add_sub(instr: Instruction, remaining: List[Instruction]): (List[Instruction], List[Instruction]) = {
    (List(instr), remaining)
  }

  /* Combines instructions of the form
      mov r0, r1
      mov r2, r0
      =>
      mov r2, r1
  */
  def combine_mov_mov(instr: Instruction, remaining: List[Instruction]): (List[Instruction], List[Instruction]) = {
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
  def redundant_str_ldr(instr: Instruction, remaining: List[Instruction]): (List[Instruction], List[Instruction]) = {
    (List(instr), remaining)
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