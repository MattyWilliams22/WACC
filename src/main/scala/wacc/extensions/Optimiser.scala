package wacc.extensions

import scala.collection.mutable.ListBuffer

import wacc.ASTNodes.Program
import wacc.backend._
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

  def registerOptimise(instrs: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    printInstructions(instrs)

    val cfg = new ControlFlowGraph()
    cfg.buildCFG(instrs)
    analyseControlFlowGraph(cfg)
    cfg.setLiveInsAndOuts()
    cfg.printCFG()

    val interferenceGraph = new InterferenceGraph()
    interferenceGraph.buildInterferenceGraph(cfg)
    interferenceGraph.printInterferenceGraph()
    val colourMap = interferenceGraph.getColourMap
    printColourMap(colourMap)

    val registerMapping = new RegisterMapping(colourMap)
    val newInstrs = registerMapping.replaceInstructions(instrs)
    printInstructions(newInstrs)
    newInstrs
  }

  private def printColourMap(colourMap: Map[Register, RegisterLocation]): Unit = {
    for ((reg, colour) <- colourMap) {
      println(reg.toString() + " -> " + colour.reg.toString())
    }
  }

  private def printInstructions(instrs: ListBuffer[Instruction]): Unit = {
    for (instr <- instrs) {
      println(instr)
    }
  }

  /* Removes all comments from the list of instructions */
  def removeComments(instructions: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    instructions.filterNot(instr => instr.isInstanceOf[Comment])
  }
}
