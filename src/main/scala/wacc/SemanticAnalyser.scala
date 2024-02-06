package wacc

import wacc.ASTNodes._

import scala.collection.mutable

class SemanticAnalyser(program: Program) {
  val symbolTable: SymbolTable = program.symbolTable

  def analyse(): Unit = {
    println("Running Semantic Analysis...")
    symbolTable.generateSymbolTable(program)
    //printMap(symbolTable.map)
  }

  def printMap[K, V](map: mutable.Map[K, V]): Unit = {
    for ((key, value) <- map) {
      println(s"Key: $key, Value: $value")
    }
  }
}
