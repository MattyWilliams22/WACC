package wacc

import wacc.ASTNodes._

class SemanticAnalyser(program: Program) {
  val symbolTable: SymbolTable = program.symbolTable

  def analyse(): Unit = {
    println("Running Semantic Analysis...")
    symbolTable.generateSymbolTable(program)
    program.check()
  }
}
