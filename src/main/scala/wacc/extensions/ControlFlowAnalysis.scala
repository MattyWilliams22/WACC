package wacc.extensions

import scala.collection.mutable.ListBuffer
import scala.collection.mutable

import wacc.ASTNodes._
import wacc.backend._

object ControlFlowAnalysis {

  def analyseProgram(prog: Program): Program = {
    Program(prog.functions.map(analyseFunction), analyseStatement(prog.statement))
  }

  private def analyseFunction(func: Function): Function = {
    Function(func._type, func.ident, func.param_list, analyseStatement(func.body))
  }

  private def analyseStatement(stmt: Statement): Statement = {
    stmt match {
      case If(cond, thenS, elseS) =>
        cond match {
          case Bool("true") => analyseStatement(thenS)
          case Bool("false") => analyseStatement(elseS)
          case _ => If(cond, analyseStatement(thenS), analyseStatement(elseS))
        }

      case While(cond, stmt) =>
        cond match {
          case Bool("true") => While(cond, analyseStatement(stmt))
          case Bool("false") => Skip()
          case _ => While(cond, analyseStatement(stmt))
        }

      case Statements(stmts) =>
        Statements(stmts.map(analyseStatement))

      case _ => stmt
    }
  }
  
  def analyseControlFlowGraph(cfg: ControlFlowGraph): Unit = {
    var allSuccs: ListBuffer[CFGNode] = ListBuffer()
    for (cfgNode <- cfg.cfgNodes) {
      allSuccs ++= cfgNode.succs
    }
    val reached = reachableFromStart(cfg)
    val nodesToRemove = cfg.cfgNodes diff reached
    for (node <- nodesToRemove) {
      node.instr match {
        case Some(AscizInstr(_, _)) => 
        case Some(Command(_, _)) =>
        case Some(Comment(_, _)) =>
        case Some(NewLine()) =>
        case _ => cfg.cfgNodes -= node
      }
    }
  }

  private def reachableFromStart(cfg: ControlFlowGraph): Set[CFGNode] = {
    val visited = mutable.Set[CFGNode]()
    val stack = mutable.Stack[CFGNode]()

    cfg.startNode.foreach(stack.push)

    while (stack.nonEmpty) {
      val node = stack.pop()
      if (!visited.contains(node)) {
        visited.add(node)
        node.succs.foreach(stack.push)
      }
    }

    visited.toSet
  }
}
