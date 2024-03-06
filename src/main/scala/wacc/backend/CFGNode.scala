package wacc.backend

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class CFGNode(val id: Int) {
  val uses = mutable.Set[Register]()
  val defs = mutable.Set[Register]()
  val succs = mutable.Set[CFGNode]()
  var liveIn = mutable.Set[Register]()
  var liveOut = mutable.Set[Register]()
}
