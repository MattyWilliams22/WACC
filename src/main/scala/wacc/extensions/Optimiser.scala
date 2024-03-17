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

  def controlFlowOptimise(main: ListBuffer[Instruction], 
                          stdLib: ListBuffer[Instruction], 
                          predef: ListBuffer[Instruction]): 
                            (ListBuffer[Instruction], 
                             ListBuffer[Instruction], 
                             ListBuffer[Instruction]) = {
    val controlFlowGraph = new ControlFlowGraph
    controlFlowGraph.addToCFG(main)
    controlFlowGraph.addToCFG(stdLib)
    controlFlowGraph.addToCFG(predef)
    analyseControlFlowGraph(controlFlowGraph)
    controlFlowGraph.printCFG(false)
    val newMain = controlFlowGraph.makeInstructions()
    val newStdLib = controlFlowGraph.makeInstructions()
    val newPredef = controlFlowGraph.makeInstructions()
    checkImports(newMain, newStdLib, newPredef)
    shrinkNewLines(newMain)
    shrinkNewLines(newStdLib)
    shrinkNewLines(newPredef)
    (newMain, newStdLib, newPredef)
  }

  def controlFlowOptimise(ast: Program): Program = {
    analyseProgram(ast)
  }

  def registerOptimise(main: ListBuffer[Instruction], 
                       stdLib: ListBuffer[Instruction], 
                       predef: ListBuffer[Instruction]): 
                      (ListBuffer[Instruction], 
                       ListBuffer[Instruction], 
                       ListBuffer[Instruction]) = {
    printInstructions(main)

    val cfg = new ControlFlowGraph()
    val mainRange = cfg.addToCFG(main)
    val stdLibRange = cfg.addToCFG(stdLib)
    val predefRange = cfg.addToCFG(predef)
    analyseControlFlowGraph(cfg)
    cfg.setLiveInsAndOuts()
    cfg.printCFG(true)

    val interferenceGraph = new InterferenceGraph()
    interferenceGraph.buildInterferenceGraph(cfg)
    interferenceGraph.printInterferenceGraph()
    val colourMap = interferenceGraph.getColourMap
    printColourMap(colourMap)

    val registerMapping = new RegisterMapping(colourMap)
    val newMain = registerMapping.mapInstructions(cfg, mainRange)
    val newStdLib = registerMapping.mapInstructions(cfg, stdLibRange)
    val newPredef = registerMapping.mapInstructions(cfg, predefRange)
    checkImports(newMain, newStdLib, newPredef)
    shrinkNewLines(newMain)
    shrinkNewLines(newStdLib)
    shrinkNewLines(newPredef)
    printInstructions(newMain)
    (newMain, newStdLib, newPredef)
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

  private def checkImports(main: ListBuffer[Instruction], stdLib: ListBuffer[Instruction], predef: ListBuffer[Instruction]): Unit = {
    if (emptyList(predef) && stdLib.nonEmpty) {
      stdLib.remove(0)
    }
    if (emptyList(stdLib) && main.nonEmpty) {
      main.remove(0)
      if (!emptyList(predef)) {
        main.prepend(Command("include \"predefinedFunctions.s\"", 0))
      }
    }
  }

  private def emptyList(instructions: ListBuffer[Instruction]): Boolean = {
    var empty = true
    for (instr <- instructions) {
      instr match {
        case NewLine() => 
        case _ => empty = false
      }
    }
    empty
  }

  private def shrinkNewLines(instructions: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    var prev: Instruction = NewLine()
    for (i <- instructions.indices.reverse) {
      val instr = instructions(i)
      prev match {
        case NewLine() => 
          instr match {
            case NewLine() => instructions.remove(i)
            case _ => prev = instr
          }
        case _ => prev = instr
      }
    }
    instructions
  }
}
