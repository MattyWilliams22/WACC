package wacc.extensions

import scala.collection.mutable

import wacc.backend.Register
import wacc.backend.Instruction

class CFGNode(val id: Int, var instr: Option[Instruction] = None) {
  val uses: mutable.Set[Register] = mutable.Set[Register]()
  val defs: mutable.Set[Register] = mutable.Set[Register]()
  val succs: mutable.Set[CFGNode] = mutable.Set[CFGNode]()
  val liveIn: mutable.Set[Register] = mutable.Set[Register]()
  val liveOut: mutable.Set[Register] = mutable.Set[Register]()
}
