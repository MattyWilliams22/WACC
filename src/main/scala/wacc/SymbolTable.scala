package wacc

import wacc.ASTNodes._

import scala.collection.mutable

class SymbolTable[Type](val parent: Option[SymbolTable[Type]],
                     val map: mutable.Map[String, Type] = mutable.Map.empty[String, Type]) {

  var topLevelSize = 0

  def add(name: String, _type: Type): Unit = {
    map.addOne(name, _type)
    incrementTotalCount()

//    _type match {
//      case function: Function => {
//        val newSymbolTable = new SymbolTable[Type](Option(this), map)
//        for (param <- function.param_list) {
//          newSymbolTable.add(param.ident.str, param._type)
//        }
//      }
//    }
  }

  def lookup(name: String): Option[Type] = map.get(name)

  def lookupInterative(name: String): Option[Type] = {
    var table: Option[SymbolTable[Type]] = Option(this)

    while (table.isDefined) {
      val res = table.get.lookup(name)
      if (res.isDefined) {
        return res
      }
      table = table.get.getParent()
    }
    None
  }

  def getParent(): Option[SymbolTable[Type]] = parent

  def incrementCount(): Unit = topLevelSize += 1

  def getCount(): Int = topLevelSize

  def incrementTotalCount(): Unit = {
    var table: Option[SymbolTable[Type]] = Option(this)
    while (table.get.getParent().isDefined) {
      table = table.get.getParent()
    }
    table.get.incrementCount()
  }

  def getTotalCount(): Int = {
    var table: Option[SymbolTable[Type]] = Option(this)
    while (table.get.getParent().isDefined) {
      table = table.get.getParent()
    }
    table.get.getCount()
  }
}
