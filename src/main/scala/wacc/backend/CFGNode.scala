package wacc.backend

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class CFGNode(val id: Int) {
  val uses = mutable.Set[Register]()
  val defs = mutable.Set[Register]()
  val succs = mutable.Set[CFGNode]()
  val liveIn = mutable.Set[Register]()
  val liveOut = mutable.Set[Register]()
}
