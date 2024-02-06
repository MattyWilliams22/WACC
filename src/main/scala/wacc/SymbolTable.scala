package wacc

import wacc.ASTNodes._

import scala.collection.mutable

class SymbolTable(var parent: Option[SymbolTable],
                     val map: mutable.Map[String, ASTNode] =
                     mutable.Map.empty[String, ASTNode]) {

  var topLevelSize = 0

  def add(name: String, node: ASTNode): Unit = {
    map.put(name, node)
    incrementTotalCount()
  }

  def generateSymbolTable(node: ASTNode): Unit = {
    node match {
      case prog: Program =>
        for (func <- prog.funcs) {
          add(func.ident.str, func)
          val symbolTable = func.symbolTable
          symbolTable.parent = Option(this)
          generateSymbolTable(func)
        }
        generateSymbolTable(prog.statement)

      case func: Function =>
        val symbolTable = func.symbolTable
        symbolTable.parent = Option(this)

        for (param <- func.param_list) {
          symbolTable.add(func.ident.str, param)
        }

        symbolTable.generateSymbolTable(func.body)

      case Statements(stmts) =>
        stmts.foreach(generateSymbolTable)

      case Declare(_, ident, _) =>
        add(ident.str, node)

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

  def lookup(name: String): Option[ASTNode] = map.get(name)

  def lookupAll(name: String): Option[ASTNode] = {
    var table: Option[SymbolTable] = Option(this)

    while (table.isDefined) {
      val res = table.get.lookup(name)
      if (res.isDefined) {
        return res
      }
      table = getParent()
    }
    None
  }

  def getParent(): Option[SymbolTable] = parent

  def incrementCount(): Unit = topLevelSize += 1

  def getCount(): Int = topLevelSize

  def incrementTotalCount(): Unit = {
    var table: Option[SymbolTable] = Option(this)
    while (table.get.getParent().isDefined) {
      table = table.get.getParent()
    }
    table.get.incrementCount()
  }

  def getTotalCount(): Int = {
    var table: Option[SymbolTable] = Option(this)
    while (table.get.getParent().isDefined) {
      table = table.get.getParent()
    }
    table.get.getCount()
  }
}
