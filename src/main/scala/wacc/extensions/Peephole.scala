package wacc.extensions

import scala.collection.mutable.ListBuffer

import wacc.backend._

object Peephole {
  def optimise(instructions: List[Instruction]): List[Instruction] = {
    instructions match {
      case Mov(dest, src, _) :: tail if dest == src =>
        optimise(tail)
      case AddInstr(_, _, ImmVal(0), _) :: tail =>
        optimise(tail)
      case SubInstr(_, _, ImmVal(0), _) :: tail =>
        optimise(tail)
      case head :: tail =>
        head :: optimise(tail)
      case Nil =>
        Nil
    }
  }
}
