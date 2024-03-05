package wacc.extensions

import scala.collection.mutable.ListBuffer

import wacc.backend._

object Peephole {
  def optimise(instructions: List[Instruction]): List[Instruction] = {
    var optimisedInstructions = redundant_mov(instructions)
    optimisedInstructions = redundant_sub_add(optimisedInstructions)
    optimisedInstructions
  }

  /* Removes instructions of the form:
      mov r0, r0
  */
  def redundant_mov(instructions: List[Instruction]): List[Instruction] = {
    instructions match {
      case Nil => Nil
      case head :: tail => head match {
        case Mov(dest, src, _) if dest == src => redundant_mov(tail)
        case _ => head :: redundant_mov(tail)
      }
    }
  }

  /* Removes instructions of the form:
      add r0, r0, #0
      sub r0, r0, #0
  */
  def redundant_sub_add(instructions: List[Instruction]): List[Instruction] = {
    instructions match {
      case Nil => Nil
      case head :: tail => head match {
        case AddInstr(dest, src, ImmVal(0), noCondition) => redundant_sub_add(tail)
        case SubInstr(dest, src, ImmVal(0), noCondition) => redundant_sub_add(tail)
        case _ => head :: redundant_sub_add(tail)
      }
    }
  }

  /* Removes instructions of the form:
      cmp r0, r0
  */
  def redundant_cmp(instructions: List[Instruction]): List[Instruction] = {
    instructions match {
      case Nil => Nil
      case head :: tail => head match {
        case CmpInstr(dest, src) if dest == src => redundant_cmp(tail)
        case _ => head :: redundant_cmp(tail)
      }
    }
  }

  /* Combine instructions with mov and add/sub
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
  def combine_mov_add_sub(instructions: List[Instruction]): List[Instruction] = {
    instructions
  }

  /* Combine instructions with mov and other instructions
      mov r0, r1
      cmp r0, #1
      =>
      cmp r1, #1
    OR
      mov r0, r1
      bic r0, r0, #1
      =>
      bic r0, r1, #1
    OR
      mov r0, r1
      mov r2, r0
      =>
      mov r2, r1
    etc.
  */
  def combine_mov(instructions: List[Instruction]): List[Instruction] = {
    instructions
  }

  /* Remove redundant str ldr pairs
      str r0, [r1]
      ldr r0, [r1]
    OR
      ldr r0, [r1]
      str r0, [r1]
  */
  def redundant_str_ldr(instructions: List[Instruction]): List[Instruction] = {
    instructions
  }

}
