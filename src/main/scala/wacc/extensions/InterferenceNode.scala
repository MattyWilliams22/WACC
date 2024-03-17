package wacc.extensions

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import wacc.backend._

class InterferenceNode(val reg: Register) {
  var neighbours = mutable.Set[Register]()

  private def addNeighbour(reg: Register): Unit = {
    if (reg != this.reg) {
      neighbours += reg
    }
  }

  def addNeighbours(regs: Set[Register]): Unit = {
    for (reg <- regs) {
      addNeighbour(reg)
    }
  }
}
