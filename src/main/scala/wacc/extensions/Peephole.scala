package wacc.extensions

import wacc.backend._

import scala.annotation.tailrec

object Peephole {

  /* Represents a transformation that can be applied to a list of instructions */
  private type Transformation = (Instruction, List[Instruction]) => (List[Instruction], Boolean)

  /* List of transformations to apply to the list of instructions */
  private val transformations: List[Transformation] = List(removeRedundantSubAdd, removeRedundantCmp, removeRedundantMov, combineMovAdd, combineDoubleMov, removeRedundantStrLdr, removeRedundantLdr, combineMovSub)

  /* Optimises instructions using the given transformations */
  def optimise(instr: Instruction, remaining: List[Instruction], transformations: List[Transformation] = transformations): (List[Instruction], List[Instruction]) = {
    var changed = true
    var currentInstr = instr
    var remainingInstrs = remaining

    /* Apply transformations until no more changes are made, allowing for transformations to be repeated and cascaded */
    while (changed) {
      changed = false

      for (transformation <- transformations) {
        val (newInstrs, wasChanged) = transformation(currentInstr, remainingInstrs)

        if (wasChanged) {
          changed = true
          currentInstr = newInstrs.head
          remainingInstrs = newInstrs.tail
        }
      }
    }

    (List(currentInstr), remainingInstrs)
  }

  /* Removes instructions of the form
      add r0, r0, 0
      sub r0, r0, 0
  */
  private def removeRedundantSubAdd(instr: Instruction, remaining: List[Instruction]): (List[Instruction], Boolean) = {
    instr match {
      case AddInstr(dest, src, ImmVal(0), _) if dest == src => (remaining, true)
      case SubInstr(dest, src, ImmVal(0), _) if dest == src => (remaining, true)
      case _ => (instr :: remaining, false)
    }
  }

  /* Removes instructions of the form
      cmp r0, 0
      cmp r0, 0
  */
  private def removeRedundantCmp(instr: Instruction, remaining: List[Instruction]): (List[Instruction], Boolean) = {
    (instr, getInstruction(remaining)) match {
      case (CmpInstr(dest1, ImmVal(0)), Some(CmpInstr(dest2, ImmVal(0)))) if dest1 == dest2 => (remaining, true)
      case _ => (instr :: remaining, false)
    }
  }

  /* Removes instructions of the form
      mov r0, r0
  */
  private def removeRedundantMov(instr: Instruction, remaining: List[Instruction]): (List[Instruction], Boolean) = {
    instr match {
      case Mov(dest, src, _) if dest == src => (remaining, true)
      case _ => (instr :: remaining, false)
    }
  }

  /* Combines instructions of the form
      mov r1, #1
      add r0, r0, r1
      =>
      add r0, r0, #1
  */
  private def combineMovAdd(instr: Instruction, remaining: List[Instruction]): (List[Instruction], Boolean) = {
    (instr, getInstruction(remaining)) match {
      case (Mov(dest1, ImmVal(value), _), Some(AddInstr(dest2, op1, op2, updateFlags))) if dest1 == op2 =>
        (AddInstr(dest2, op1, ImmVal(value), updateFlags) :: dropInstructions(remaining, 1), true)
      case _ => (instr :: remaining, false)
    }
  }

  /* Combines instructions of the form
      mov r1, #1
      sub r0, r0, r1
      =>
      sub r0, r0, #1
  */
  private def combineMovSub(instr: Instruction, remaining: List[Instruction]): (List[Instruction], Boolean) = {
    (instr, getInstruction(remaining)) match {
      case (Mov(dest1, ImmVal(value), _), Some(SubInstr(dest2, op1, op2, updateFlags))) if dest1 == op2 =>
        (SubInstr(dest2, op1, ImmVal(value), updateFlags) :: dropInstructions(remaining, 1), true)
      case _ => (instr :: remaining, false)
    }
  }

  /* Combines instructions of the form
      mov r0, r1
      mov r2, r0
      =>
      mov r2, r1
  */
  private def combineDoubleMov(instr: Instruction, remaining: List[Instruction]): (List[Instruction], Boolean) = {
    (instr, getInstruction(remaining), getInstruction(remaining, 1)) match {
      case (Comment("Start of identifier ", _), Some(Mov(dest1, src1, cond1)), Some(Mov(dest2, src2, cond2))) if dest1 == src2 && cond1 == cond2 =>
        (Mov(dest2, src1, cond1) :: dropInstructions(remaining, 2), true)
      case _ => (instr :: remaining, false)
    }
  }

  /* Remove redundant str ldr pairs
      str r0, [r1]
      ldr r0, [r1]
    OR
      ldr r0, [r1]
      str r0, [r1]
  */
  private def removeRedundantStrLdr(instr: Instruction, remaining: List[Instruction]): (List[Instruction], Boolean) = {
    (instr, getInstruction(remaining)) match {
      case (instr1@StrInstr(dest1, op1, _), Some(Ldr(dest2, op2))) if dest1 == dest2 && op1 == op2 =>
        (instr1 :: dropInstructions(remaining, 1), true)
      case (instr1@Ldr(dest1, op1), Some(StrInstr(dest2, op2, _))) if dest1 == dest2 && op1 == op2 =>
        (instr1 :: dropInstructions(remaining, 1), true)
      case _ => (instr :: remaining, false)
    }
  }

  /* Removes the first ldr instruction when instructions are of the form
      ldr r0, [r1]
      ldr r0, [r2]
      =>
      ldr r0, [r2]
  */
  private def removeRedundantLdr(instr: Instruction, remaining: List[Instruction]): (List[Instruction], Boolean) = {
    (instr, getInstruction(remaining)) match {
      case (Ldr(dest1, _), Some(Ldr(dest2, op2))) if dest1 == dest2 =>
        (Ldr(dest1, op2) :: dropInstructions(remaining, 1), true)
      case _ => (instr :: remaining, false)
    }
  }

  /* Gets the nth non-comment instruction. By default, n = 0 */
  @tailrec
  private def getInstruction(remaining: List[Instruction], n: Int = 0): Option[Instruction] = {
    remaining match {
      case Nil => None
      case Comment(_, _) :: tail => getInstruction(tail, n)
      case _ :: tail if n > 0 => getInstruction(tail, n - 1)
      case head :: _ => Some(head)
    }
  }

  /* Drops n instructions from the list */
  @tailrec
  private def dropInstructions(remaining: List[Instruction], n: Int): List[Instruction] = {
    remaining match {
      case Nil => Nil
      case Comment(_, _) :: tail => dropInstructions(tail, n)
      case _ :: tail if n > 0 => dropInstructions(tail, n - 1)
      case _ => remaining
    }
  }
}