package wacc.extensions

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import wacc.backend.Register
import wacc.backend.Instruction

class CFGNode(val id: Int, var instr: Option[Instruction] = None) {
  val uses = mutable.Set[Register]()
  val defs = mutable.Set[Register]()
  val succs = mutable.Set[CFGNode]()
  var liveIn = mutable.Set[Register]()
  var liveOut = mutable.Set[Register]()
}
