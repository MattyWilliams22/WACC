package wacc.backend

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import wacc.backend.RegisterMapping

object GraphColouring {

  def colourInstructions(instrs: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    printInstructions(instrs)

    val cfg = new ControlFlowGraph()
    cfg.buildCFG(instrs)
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

  private def printColourMap(colourMap: Map[Register, Register]): Unit = {
    for ((reg, colour) <- colourMap) {
      println(reg.toString() + " -> " + colour)
    }
  }

  private def printInstructions(instrs: ListBuffer[Instruction]): Unit = {
    for (instr <- instrs) {
      println(instr)
    }
  }
}
