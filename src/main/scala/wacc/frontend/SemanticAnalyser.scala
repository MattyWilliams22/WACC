package wacc.frontend

import wacc.ASTNodes._
import wacc.Main.SEMANTIC_ERR_CODE
import wacc.SymbolTable

class SemanticAnalyser(program: Program, stdLibSymbolTable: Option[SymbolTable]) {
  val symbolTable: SymbolTable = program.symbolTable

  def analyse(): Unit = {
    println("Running Semantic Analysis...")
    symbolTable.generateSymbolTable(program)
    symbolTable.parent = stdLibSymbolTable
    val semanticErrors: List[SemanticError] = program.check()
    if (semanticErrors.nonEmpty) {
      semanticErrors.foreach(e => e.print())
      System.exit(SEMANTIC_ERR_CODE)
    }
  }
}

case class SemanticError(msg: String, node: ASTNode) {
  def print(): Unit = {
    println("Semantic Error in ASTNode: " + node)
    println(s"  $msg\n")
  }
}
