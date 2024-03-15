package wacc.extensions

import scala.collection.mutable.ListBuffer
import scala.collection.mutable

import wacc.ASTNodes._
import wacc.backend._

object ControlFlowAnalysis {
  
  /* Analyse the program to remove unreachable code within while loops and if statements */
  def analyseProgram(prog: Program): Program = {
    Program(prog.functions.map(analyseFunction), analyseStatement(prog.statement))
  }
  
  /* Analyse the function to remove unreachable code within while loops and if statements */
  private def analyseFunction(func: Function): Function = {
    Function(func._type, func.ident, func.param_list, analyseStatement(func.body))
  }
  
  /* Analyse the statement to remove unreachable code within while loops and if statements, by checking the condition of the statement */
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
  
  /* Analyse the condition expression of an if statement or while loop to see if it is
  a constant value */
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

  /* Analyse the comparison of two expressions to see if the result is a constant value */
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

  /* Analyse the less than expression to see if the result is a constant value */
  private def analyseLT(exp1: Expr, exp2: Expr): Expr = {
    analyseComparison(exp1, exp2, _ < _) match {
      case Some(expr) => expr
      case None => LT(analyseExpr(exp1), analyseExpr(exp2))
    }
  }

  /* Analyse the less than or equal to expression to see if the result is a constant value */
  private def analyseLTEQ(exp1: Expr, exp2: Expr): Expr = {
    analyseComparison(exp1, exp2, _ <= _) match {
      case Some(expr) => expr
      case None => LTEQ(analyseExpr(exp1), analyseExpr(exp2))
    }
  }

  /* Analyse the greater than expression to see if the result is a constant value */
  private def analyseGT(exp1: Expr, exp2: Expr): Expr = {
    analyseComparison(exp1, exp2, _ > _) match {
      case Some(expr) => expr
      case None => GT(analyseExpr(exp1), analyseExpr(exp2))
    }
  }

  /* Analyse the greater than or equal to expression to see if the result is a constant value */
  private def analyseGTEQ(exp1: Expr, exp2: Expr): Expr = {
    analyseComparison(exp1, exp2, _ >= _) match {
      case Some(expr) => expr
      case None => GTEQ(analyseExpr(exp1), analyseExpr(exp2))
    }
  }
  
  def analyseControlFlowGraph(cfg: ControlFlowGraph): Unit = {
    removeUnusedNodes(cfg)
    simplifyBranches(cfg)
  }

  private def removeUnusedNodes(cfg: ControlFlowGraph): Unit = {
    val reached = reachableFromStart(cfg)
    val nodesToRemove = cfg.cfgNodes diff reached
    for (node <- nodesToRemove) {
      node.instr match {
        case Some(AscizInstr(_, _)) =>
        case Some(NewLine()) =>
        case Some(Command("align 4", _)) =>
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

  private def simplifyBranches(cfg: ControlFlowGraph): Unit = {
    val nodes = cfg.cfgNodes.toList
    for (node <- nodes) {
      checkJumpToUnconditionalJump(cfg, node)
      checkJumpToNextInstruction(cfg, node)
    }
  }

  private def checkJumpToUnconditionalJump(cfg: ControlFlowGraph, node: CFGNode): Unit = {
    node.instr match {
      case Some(BInstr(target, cond, link)) if cond != noCondition =>
        val labelNode = cfg.labelToNode(target)
        val nextNode = labelNode.succs.head
        nextNode.instr match {
          case Some(BInstr(newTarget, noCondition, _)) =>
            node.instr = Some(BInstr(newTarget, cond, link))
            node.succs -= labelNode
            node.succs ++= labelNode.succs
          case _ =>
        }
      case _ =>
    }
  }

  private def checkJumpToNextInstruction(cfg: ControlFlowGraph, node: CFGNode): Unit = {
    node.instr match {
      case Some(BInstr(target, cond, link)) =>
        val labelNode = cfg.labelToNode.get(target)
        if (labelNode.isDefined && labelNode.get.id == node.id + 1) {
          val prevNode = cfg.getCFGNode(node.id - 1)
          prevNode.succs -= node
          prevNode.succs += labelNode.get
          cfg.cfgNodes -= node
        }
      case _ =>
    }
  }
}
