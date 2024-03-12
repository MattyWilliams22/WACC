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
  
  private def analyseExpr(expr: Expr): Expr = {
    expr match {
      case And(Bool("true"), Bool("true")) => Bool("true")
      case And(Bool("true"), Bool("false")) => Bool("false")
      case And(Bool("false"), Bool("true")) => Bool("false")
      case And(Bool("false"), Bool("false")) => Bool("false")
      case And(exp1, exp2) => And(analyseExpr(exp1), analyseExpr(exp2))
      case Or(Bool("true"), Bool("true")) => Bool("true")
      case Or(Bool("true"), Bool("false")) => Bool("true")
      case Or(Bool("false"), Bool("true")) => Bool("true")
      case Or(Bool("false"), Bool("false")) => Bool("false")
      case Or(exp1, exp2) => Or(analyseExpr(exp1), analyseExpr(exp2))
      case Not(Bool("true")) => Bool("false")
      case Not(Bool("false")) => Bool("true")
      case Not(exp) => Not(analyseExpr(exp))
      case EQ(exp1, exp2) => 
      if (exp1 == exp2) {
        Bool("true")
      } else {
        Bool("false")
      }
      case NEQ(exp1, exp2) => 
      if (exp1 != exp2) {
        Bool("true")
      } else {
        Bool("false")
      }
      case LT(exp1, exp2) => analyseLT(analyseExpr(exp1), analyseExpr(exp2))
      case GT(exp1, exp2) => analyseGT(analyseExpr(exp1), analyseExpr(exp2))
      case LTEQ(exp1, exp2) => analyseLTEQ(analyseExpr(exp1), analyseExpr(exp2))
      case GTEQ(exp1, exp2) => analyseGTEQ(analyseExpr(exp1), analyseExpr(exp2))
      case _ => expr
    }
  }
  
  private def analyseLT(exp1: Expr, exp2: Expr): Expr = {
    (exp1, exp2) match {
      case (Ch(c1), Ch(c2)) => 
      if (c1 < c2) {
        Bool("true")
      } else {
        Bool("false")
      }
      case (Num(n1), Num(n2)) => 
      if (n1 < n2) {
        Bool("true")
      } else {
        Bool("false")
      }
      case _ => LT(exp1, exp2)
    }
  }

  private def analyseGT(exp1: Expr, exp2: Expr): Expr = {
    (exp1, exp2) match {
      case (Ch(c1), Ch(c2)) => 
      if (c1 > c2) {
        Bool("true")
      } else {
        Bool("false")
      }
      case (Num(n1), Num(n2)) => 
      if (n1 > n2) {
        Bool("true")
      } else {
        Bool("false")
      }
      case _ => GT(exp1, exp2)
    }
  }
  
  private def analyseLTEQ(exp1: Expr, exp2: Expr): Expr = {
    (exp1, exp2) match {
      case (Ch(c1), Ch(c2)) => 
      if (c1 <= c2) {
        Bool("true")
      } else {
        Bool("false")
      }
      case (Num(n1), Num(n2)) => 
      if (n1 <= n2) {
        Bool("true")
      } else {
        Bool("false")
      }
      case _ => LTEQ(exp1, exp2)
    }
  }
  
  private def analyseGTEQ(exp1: Expr, exp2: Expr): Expr = {
    (exp1, exp2) match {
      case (Ch(c1), Ch(c2)) => 
      if (c1 >= c2) {
        Bool("true")
      } else {
        Bool("false")
      }
      case (Num(n1), Num(n2)) => 
      if (n1 >= n2) {
        Bool("true")
      } else {
        Bool("false")
      }
      case _ => GTEQ(exp1, exp2)
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
