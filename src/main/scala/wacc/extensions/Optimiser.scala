package wacc.extensions

import wacc.backend.Instruction
import wacc.extensions.Peephole._
import scala.collection.mutable.ListBuffer

object Optimiser {

  def optimiseInstructions(instructions: List[Instruction]): ListBuffer[Instruction] = {
    instructions match {
      case Nil => ListBuffer.from(List())
      case instr :: remaining =>
        val transformed = ListBuffer.from(optimise(instr, remaining))
        transformed ++= optimiseInstructions(remaining)
    }
  }
}
