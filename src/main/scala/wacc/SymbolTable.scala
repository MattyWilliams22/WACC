package wacc

import scala.collection.mutable

import wacc.ASTNodes._
import wacc.Main.SEMANTIC_ERR_CODE

class SymbolTable(var parent: Option[SymbolTable],
                  var canAccessVars: Boolean = true,
                  val funcMap: mutable.Map[String, Function] = mutable.Map.empty[String, Function],
                  val varMap: mutable.Map[String, ASTNode] = mutable.Map.empty[String, ASTNode]) {

  def addFunction(name: String, node: Function): Unit = {
    funcMap.put(name, node)
  }

  def addVariable(name: String, node: ASTNode): Unit = {
    varMap.put(name, node)
  }

  def generateSymbolTable(node: ASTNode): Unit = {
    node match {
      case prog: Program =>
        for (func <- prog.functions) {
          if (lookupFunction(func.ident.str).isDefined) {
            System.exit(SEMANTIC_ERR_CODE)
          }
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
            System.exit(SEMANTIC_ERR_CODE)
          }
          addVariable(param.ident.str, param)
        }
        func.bodySymbolTable.generateSymbolTable(func.body)

      case Declare(_, ident, _) =>
        if (lookupVariable(ident.str).isDefined) {
          System.exit(SEMANTIC_ERR_CODE)
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

  def lookupFunction(name: String): Option[Function] = funcMap.get(name)
  def lookupVariable(name: String): Option[ASTNode] = varMap.get(name)

  def lookupAllVariables(name: String): Option[ASTNode] = {
    var table: Option[SymbolTable] = Option(this)
    if (canAccessVars) {
      while (table.isDefined) {
        val res = table.get.lookupVariable(name)
        if (res.isDefined) {
          return res
        }
        table = table.get.getParent()
      }
      None
    } else {
      table.get.lookupVariable(name)
    }
  }

  def lookupAllFunctions(name: String): Option[Function] = {
    var table: Option[SymbolTable] = Option(this)

    while (table.isDefined) {
      val res = table.get.lookupFunction(name)
      if (res.isDefined) {
        return res
      }
      table = table.get.getParent()
    }
    None
  }

  private def getParent(): Option[SymbolTable] = parent
}

