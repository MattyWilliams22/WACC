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
      analyseExpr(cond) match {
        case Bool("true") => analyseStatement(thenS)
        case Bool("false") => analyseStatement(elseS)
        case _ => If(analyseExpr(cond), analyseStatement(thenS), analyseStatement(elseS))
      }
      
      case While(cond, stmt) =>
      analyseExpr(cond) match {
        case Bool("false") => Skip()
        case _ => While(analyseExpr(cond), analyseStatement(stmt))
      }
      
      case Statements(stmts) =>
      Statements(stmts.map(analyseStatement))
      
      case _ => stmt
    }
  }
  
  private def analyseExpr(expr: Expr): Expr = {
    expr match {
      case And(exp1, exp2) =>
        analyseExpr(exp1) match {
          case Bool("true") => analyseExpr(exp2)
          case Bool("false") => Bool("false")
          case _ => And(analyseExpr(exp1), analyseExpr(exp2))
        }
      case Or(exp1, exp2) =>
        analyseExpr(exp1) match {
          case Bool("true") => Bool("true")
          case Bool("false") => analyseExpr(exp2)
          case _ => Or(analyseExpr(exp1), analyseExpr(exp2))
        }
      case Not(exp) =>
        analyseExpr(exp) match {
          case Bool("true") => Bool("false")
          case Bool("false") => Bool("true")
          case _ => Not(analyseExpr(exp))
        }
      case EQ(exp1, exp2) => 
        if (analyseExpr(exp1) == analyseExpr(exp2)) {
          Bool("true")
        } else {
          EQ(analyseExpr(exp1), analyseExpr(exp2))
        }
      case NEQ(exp1, exp2) => 
        if (analyseExpr(exp1) == analyseExpr(exp2)) {
          Bool("false")
        } else {
          NEQ(analyseExpr(exp1), analyseExpr(exp2))
        }
      case LT(exp1, exp2) => analyseLT(analyseExpr(exp1), analyseExpr(exp2))
      case GT(exp1, exp2) => analyseGT(analyseExpr(exp1), analyseExpr(exp2))
      case LTEQ(exp1, exp2) => analyseLTEQ(analyseExpr(exp1), analyseExpr(exp2))
      case GTEQ(exp1, exp2) => analyseGTEQ(analyseExpr(exp1), analyseExpr(exp2))
      case _ => expr
    }
  }

  private def analyseComparison(exp1: Expr, exp2: Expr, comparison: (Int, Int) => Boolean): Option[Expr] = {
    (analyseExpr(exp1), analyseExpr(exp2)) match {
      case (Ch(c1), Ch(c2)) => 
        if (comparison(c1.toInt, c2.toInt)) {
          Some(Bool("true"))
        } else {
          Some(Bool("false"))
        }
      case (Num(n1), Num(n2)) => 
        if (comparison(n1, n2)) {
          Some(Bool("true"))
        } else {
          Some(Bool("false"))
        }
      case _ => None
    }
  }

  private def analyseLT(exp1: Expr, exp2: Expr): Expr = {
    analyseComparison(exp1, exp2, _ < _) match {
      case Some(expr) => expr
      case None => LT(analyseExpr(exp1), analyseExpr(exp2))
    }
  }

  private def analyseLTEQ(exp1: Expr, exp2: Expr): Expr = {
    analyseComparison(exp1, exp2, _ <= _) match {
      case Some(expr) => expr
      case None => LTEQ(analyseExpr(exp1), analyseExpr(exp2))
    }
  }

  private def analyseGT(exp1: Expr, exp2: Expr): Expr = {
    analyseComparison(exp1, exp2, _ > _) match {
      case Some(expr) => expr
      case None => GT(analyseExpr(exp1), analyseExpr(exp2))
    }
  }

  private def analyseGTEQ(exp1: Expr, exp2: Expr): Expr = {
    analyseComparison(exp1, exp2, _ >= _) match {
      case Some(expr) => expr
      case None => GTEQ(analyseExpr(exp1), analyseExpr(exp2))
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
