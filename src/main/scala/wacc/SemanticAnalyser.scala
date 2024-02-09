package wacc

import wacc.ASTNodes._

import scala.collection.mutable

class SemanticAnalyser(program: Program) {
  val symbolTable: SymbolTable = program.symbolTable

  def analyse(): Unit = {
    println("Running Semantic Analysis...")
    symbolTable.generateSymbolTable(program)
    program.check()
  }
}
