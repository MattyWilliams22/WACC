package wacc

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import wacc.ASTNodes._
import wacc.frontend.SemanticError

class SymbolTable(var parent: Option[SymbolTable],
                  var canAccessVars: Boolean = true,
                  val funcMap: mutable.Map[String, List[Function]] = mutable.Map.empty[String, List[Function]],
                  val varMap: mutable.Map[String, ASTNode] = mutable.Map.empty[String, ASTNode]) {

  private var varCounter: Int = 0
  private var funcCounter: Int = 0
  private val topLevelSymbolTable: Option[SymbolTable] = None

  private def incrementVarCounter(): Unit = {
    varCounter += 1
    topLevelSymbolTable.foreach(_.incrementTotalVarCount())
  }

  private def addFunction(name: String, node: Function): String = {
    val uniqueName = "wacc_" + node.ident.str + "_" + funcCounter
    funcCounter += 1
    node.ident.nickname = Some(uniqueName)
    val funcs = funcMap.getOrElse(name, List())
    for (func <- funcs) {
      var matches = true
      if (func.param_list.length != node.param_list.length) {
        matches = false 
      } else {
        for (i <- func.param_list.indices) {
          if (func.param_list(i)._type != node.param_list(i)._type) {
            matches = false
          }
        }
      }
      if (node._type != func._type) {
        matches = false
      }
      if (matches) {
        semanticErrors += SemanticError(s"Function ${node.ident.str} defined more than once with the same parameters and return type", node)
      }
    }
    funcMap.put(name, node :: funcs)
    uniqueName
  }

  private def addVariable(name: String, node: ASTNode): String = {
    val uniqueName = generateUniqueVarName()
    node match {
      case d: Declare => d.ident.nickname = Some(uniqueName)
      case p: Param => p.ident.nickname = Some(uniqueName)
      case _ =>
    }
    varMap.put(name, node)
    incrementVarCounter()
    uniqueName
  }

  private def incrementTotalVarCount(): Unit = topLevelSymbolTable.foreach(_.incrementTotalVarCount())

  private def generateUniqueVarName(): String = s"var_${this}_$varCounter"

  def generateSymbolTable(node: ASTNode): Unit = {
    node match {
      case prog: Program =>
        for (func <- prog.functions) {
          addFunction(func.ident.str, func)
          val symbolTable = func.argSymbolTable
          symbolTable.parent = Option(this)
          symbolTable.canAccessVars = false
          symbolTable.generateSymbolTable(func)
        }
        generateSymbolTable(prog.statement)

      case func: Function =>
        for (param <- func.param_list) {
          if (lookupVariable(param.ident.str).isDefined) {
            semanticErrors += SemanticError(s"Parameter ${param.ident.str} defined more than once", param)
          }
          addVariable(param.ident.str, param)
        }
        func.bodySymbolTable.generateSymbolTable(func.body)

      case Declare(_, ident, _) =>
        if (lookupVariable(ident.str).isDefined) {
          semanticErrors += SemanticError(s"Variable ${ident.str} defined more than once in this scope", ident)
        }
        addVariable(ident.str, node)

      case ifStatement: If =>
        val thenBlockSymbolTable = ifStatement.thenSymbolTable
        val elseBlockSymbolTable = ifStatement.elseSymbolTable
        thenBlockSymbolTable.parent = Option(this)
        elseBlockSymbolTable.parent = Option(this)
        thenBlockSymbolTable.generateSymbolTable(ifStatement.thenS)
        elseBlockSymbolTable.generateSymbolTable(ifStatement.elseS)

      case whileLoop: While =>
        val whileBlockSymbolTable = whileLoop.symbolTable
        whileBlockSymbolTable.parent = Option(this)
        whileBlockSymbolTable.generateSymbolTable(whileLoop.body)

      case scope: Scope =>
        val symbolTable = scope.symbolTable
        symbolTable.parent = Option(this)
        symbolTable.generateSymbolTable(scope.body)

      case _ =>
    }
  }

  private def lookupFunctions(name: String): List[Function] = {
    funcMap.get(name).getOrElse(List())
  }
  private def lookupVariable(name: String): Option[ASTNode] = varMap.get(name)

  def lookupAllVariables(name: String): Option[ASTNode] = {
    var table: Option[SymbolTable] = Option(this)
    if (canAccessVars) {
      while (table.isDefined) {
        val res = table.get.lookupVariable(name)
        if (res.isDefined) {
          return res
        }
        table = table.get.getParent
      }
      None
    } else {
      table.get.lookupVariable(name)
    }
  }

  def lookupAllFunctions(name: String): List[Function] = {
    var table: Option[SymbolTable] = Option(this)

    var funcs: ListBuffer[Function] = ListBuffer()
    while (table.isDefined) {
      val res = table.get.lookupFunctions(name)
      funcs ++= res
      table = table.get.getParent
    }

    funcs.toList
  }

  private def getParent: Option[SymbolTable] = parent
}

