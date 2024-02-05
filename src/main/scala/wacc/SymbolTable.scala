package wacc

import scala.collection.mutable

class SymbolTable[A](val parent: Option[Table[A]],
                     val map: mutable.Map[String, A] = mutable.Map.empty[String, A]) extends Table[A] {

  var topLevelSize = 0

  def add(name: String, t: A): Unit = {
    map.addOne(name, t)
    incrementTotalCount()
  }

  def lookup(name: String): Option[A] = map.get(name)

  def lookupInterative(name: String): Option[A] = {
    var table: Option[Table[A]] = Option(this)

    while (table.isDefined) {
      val res = table.get.lookup(name)
      if (res.isDefined) {
        return res
      }
      table = table.get.getParent()
    }
    None
  }

  def getParent(): Option[Table[A]] = parent

  def incrementCount(): Unit = topLevelSize += 1

  def getCount(): Int = topLevelSize

  def incrementTotalCount(): Unit = {
    var table: Option[Table[A]] = Option(this)
    while (table.get.getParent().isDefined) {
      table = table.get.getParent()
    }
    table.get.incrementCount()
  }

  def getTotalCount(): Int = {
    var table: Option[Table[A]] = Option(this)
    while (table.get.getParent().isDefined) {
      table = table.get.getParent()
    }
    table.get.getCount()
  }
}
