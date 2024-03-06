package wacc.backend

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class InterferenceGraph {
  var nodes = mutable.Map[Register, InterferenceNode]()

  val allColours: Set[Register] = Set(R4, R5, R6, R7, R8, R9, R10, R1, R2, R3, IP)

  def printInterferenceGraph(): Unit = {
    for ((reg, node) <- nodes) {
      println("Node " + reg + ":")
      println("-> " + node.neighbours.mkString(", "))
    }
  }

  private def addCFGNode(node: CFGNode): Unit = {
    for (reg <- node.liveOut) {
      val interferenceNode = getInterferenceNode(reg)
      interferenceNode.addNeighbours(node.liveOut.toSet - reg)
    }
  }

  def buildInterferenceGraph(cfg: ControlFlowGraph): Unit = {
    for (node <- cfg.cfgNodes) {
      addCFGNode(node)
    }
  }

  private def getInterferenceNode(reg: Register): InterferenceNode = {
    if (nodes.contains(reg)) {
      nodes(reg)
    } else {
      val newNode = new InterferenceNode(reg)
      nodes += (reg -> newNode)
      newNode
    }
  }

  def getColourMap: Map[Register, Register] = {
    var colourMap = mutable.Map[Register, Register]()
    initColourMap(colourMap)
    for ((reg, node) <- nodes) {
      val adjacentColours: Set[Register] = node.neighbours.map(colourMap).toSet
      val availableColours: Set[Register] = allColours diff adjacentColours
      if (availableColours.nonEmpty) {
        colourMap += (reg -> availableColours.head)
      } else {
        colourMap += (reg -> reg)
      }
    }
    colourMap.toMap
  }

  private def initColourMap(colourMap: mutable.Map[Register, Register]): Unit = {
    for (reg <- allColours) {
      colourMap += (reg -> reg)
    }
  }
}
