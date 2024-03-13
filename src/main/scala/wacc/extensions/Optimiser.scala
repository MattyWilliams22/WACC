package wacc.extensions

import scala.collection.mutable.ListBuffer

import wacc.ASTNodes.Program
import wacc.backend.Instruction
import wacc.backend.Comment
import wacc.extensions.Peephole._
import wacc.extensions.ControlFlowAnalysis._

object Optimiser {

  def optimiseInstructions(instructions: List[Instruction]): ListBuffer[Instruction] = {
    instructions match {
      case Nil => ListBuffer.from(List())
      case instr :: remaining =>
        val optimisedInstr = optimise(instr, remaining)
        val transformed = ListBuffer.from(optimisedInstr._1)
        val newRemaining = optimisedInstr._2
        transformed ++= optimiseInstructions(newRemaining)
    }
  }

  def controlFlowOptimise(instructions: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    val controlFlowGraph = new ControlFlowGraph
    controlFlowGraph.buildCFG(instructions)
    analyseControlFlowGraph(controlFlowGraph)
    controlFlowGraph.printCFG()
    controlFlowGraph.makeInstructions()
  }

  def controlFlowOptimise(ast: Program): Program = {
    analyseProgram(ast)
  }

  /* Removes all comments from the list of instructions */
  def removeComments(instructions: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    instructions.filterNot(instr => instr.isInstanceOf[Comment])
  }
}
