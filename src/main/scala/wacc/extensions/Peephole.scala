package wacc.extensions

import scala.collection.mutable.ListBuffer

import wacc.backend._

object Peephole {

  type Transformation = (Instruction, List[Instruction]) => List[Instruction]

  val transformations: List[Transformation] = List(redundant_sub_add, redundant_cmp)

  def optimise(instr: Instruction, remaining: List[Instruction], transformations: List[Transformation] = transformations): List[Instruction] = {
    transformations match {
      case Nil => List(instr)
      case head :: tail =>
        head(instr, remaining) match {
          case Nil => optimise(instr, remaining, tail)
          case transformed => transformed
        }
    }
  }

  def processInstructions(instructions: List[Instruction]): List[Instruction] = {
    instructions match {
      case Nil => Nil
      case head :: tail => optimise(head, tail, transformations) ++ processInstructions(tail)
    }
  }

  /* Removes instructions of the form
      add r0, r0, 0
      sub r0, r0, 0
  */
  def redundant_sub_add(instr: Instruction, remaining: List[Instruction]): List[Instruction] = {
    instr match {
      case AddInstr(dest, src, ImmVal(0), _) if dest == src => Nil
      case SubInstr(dest, src, ImmVal(0), _) if dest == src => Nil
      case _ => List(instr)
    }
  }

  /* Removes instructions of the form
      cmp r0, 0
      cmp r0, 0
  */
  def redundant_cmp(instr: Instruction, remaining: List[Instruction]): List[Instruction] = {
    instr match {
      case CmpInstr(dest, ImmVal(0)) => remaining match {
        case Nil => List(instr)
        case head :: tail => head match {
          case CmpInstr(dest, ImmVal(0)) => Nil
          case _ => List(instr)
        }
      }
      case _ => List(instr)
    }
  }

  /* Removes instructions of the form
      mov r0, r0
  */
  def redundant_move(instr: Instruction, remaining: List[Instruction]): List[Instruction] = {
    instr match {
      case Mov(dest, src, _) if dest == src => Nil
      case _ => List(instr)
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
  def combine_mov_add_sub(instr: Instruction, remaining: List[Instruction]): List[Instruction] = {
    remaining
  }

  /* Combines instructions of the form
      mov r0, r1
      mov r2, r0
      =>
      mov r2, r1
  */
  def combine_mov_mov(instr: Instruction, remaining: List[Instruction]): List[Instruction] = {
    remaining
  }

  /* Remove redundant str ldr pairs
      str r0, [r1]
      ldr r0, [r1]
    OR
      ldr r0, [r1]
      str r0, [r1]
  */
  def redundant_str_ldr(instr: Instruction, remaining: List[Instruction]): List[Instruction] = {
    remaining
  }
}