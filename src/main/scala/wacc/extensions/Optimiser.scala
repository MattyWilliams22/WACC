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
    controlFlowGraph.printCFG()
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
