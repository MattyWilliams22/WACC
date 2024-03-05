package wacc.extensions

import wacc.backend.Instruction
import wacc.extensions.Peephole._
import scala.collection.mutable.ListBuffer

object Optimiser {
  def optimiseInstructions(instructions: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    var optimisedInstructions = instructions.toList
    var newOptimisedInstructions = optimise(optimisedInstructions)

    while (optimisedInstructions != newOptimisedInstructions) {
      optimisedInstructions = newOptimisedInstructions
      newOptimisedInstructions = optimise(optimisedInstructions)
    }
    ListBuffer.from(optimisedInstructions)
  }
}
