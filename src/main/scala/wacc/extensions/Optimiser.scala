package wacc.extensions

import wacc.backend.Instruction
import wacc.extensions.Peephole._
import scala.collection.mutable.ListBuffer

object Optimiser {

  def optimiseInstructions(instructions: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    val transformedInstructions = new ListBuffer[Instruction]()
    for (i <- 0 until instructions.length) {
      val instr = instructions(i)
      val remaining = instructions.slice(i + 1, instructions.length).toList
      val transformed = optimise(instr, remaining)
      transformedInstructions ++= transformed
    }
    transformedInstructions
  }
}
