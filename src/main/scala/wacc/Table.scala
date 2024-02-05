package wacc

trait Table[A] {
  def add(name: String, t: A): Unit
  def lookup(name: String): Option[A]
  def lookupInterative(name: String): Option[A]
  def getParent(): Option[Table[A]]
  def incrementCount(): Unit
  def getCount(): Int
  def getTotalCount(): Int
}
