package wacc.backend

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import wacc.backend.RegisterMapping

object GraphColouring {

  def colourInstructions(instrs: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    val cfg = new ControlFlowGraph()
    cfg.buildCFG(instrs)

    val interferenceGraph = new InterferenceGraph()
    interferenceGraph.buildInterferenceGraph(cfg)
    val colourMap = interferenceGraph.getColourMap

    val registerMapping = new RegisterMapping(colourMap)
    registerMapping.replaceInstructions(instrs)
  }
}
