package wacc.backend

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class ControlFlowGraph {
  object CFGNodeOrdering extends Ordering[CFGNode] {
    def compare(a: CFGNode, b: CFGNode) = a.id compare b.id
  }
  val cfgNodes = mutable.SortedSet[CFGNode]()(CFGNodeOrdering)
  var labelToNode = mutable.Map[String, CFGNode]()
  var labelReferences = mutable.Map[String, ListBuffer[CFGNode]]()
  var startNode: Option[CFGNode] = None

  private def addLabelToNode(label: String, node: CFGNode): Unit = {
    labelToNode += (label -> node)
    val isString = label.startsWith(".L.")
    if (labelReferences.contains(label)) {
      labelReferences(label).map(n => n.succs += node)
      labelReferences(label).map(n => node.succs += n)
    }
  }

  private def addLabelReference(label: String, node: CFGNode): Unit = {
    if (labelReferences.contains(label)) {
      labelReferences(label) += node
    } else {
      labelReferences += (label -> ListBuffer(node))
    }
  }

  private def checkIfOperandUsed(node: CFGNode, op: Operand): Unit = {
    op match {
      case r: Register => node.uses += r
      case RegShift(r, s) => node.uses += r
      case Addr(r, o) => 
        node.uses += r
        checkIfOperandUsed(node, o)
      case _ =>
    }
  }

  def printCFG(): Unit = {
    for (cfgNode <- cfgNodes) {
      println("Node " + cfgNode.id + ":")
      println("Uses: " + cfgNode.uses.mkString(", "))
      println("Defs: " + cfgNode.defs.mkString(", "))
      println("-> " + cfgNode.succs.map(_.id).mkString(", "))
    }
  }

  def buildCFG(instrs: ListBuffer[Instruction]): Unit = {
    var nodeId = 0

    for (instr <- instrs) {
      val node = getCFGNode(nodeId)
      node.instr = Some(instr)
      
      instr match {
        case Comment(_, _) =>
          node.succs += getCFGNode(nodeId + 1)
        case Command(str, _) => 
          str match {
            case "ltorg" =>
            case _ => 
              node.succs += getCFGNode(nodeId + 1)
          }
        case Label(name) =>
          addLabelToNode(name, node)
          node.succs += getCFGNode(nodeId + 1)
        case Push(regs) =>
          regs.map(r => node.uses += r)
          node.succs += getCFGNode(nodeId + 1)
        case Pop(regs) =>
          regs.map(r => node.defs += r)
          node.succs += getCFGNode(nodeId + 1)
        case Ldr(reg, operand) =>
          node.uses += reg
          checkIfOperandUsed(node, operand)
          node.succs += getCFGNode(nodeId + 1)
        case AdrInstr(reg, label) =>
          node.defs += reg
          node.succs += getCFGNode(nodeId + 1)
        case Mov(reg, operand, condition) =>
          node.defs += reg
          checkIfOperandUsed(node, operand)
          node.succs += getCFGNode(nodeId + 1)
        case AddInstr(reg1, reg2, operand2, updateFlags) =>
          node.defs += reg1
          node.uses += reg2
          checkIfOperandUsed(node, operand2)
          node.succs += getCFGNode(nodeId + 1)
        case SubInstr(reg1, reg2, operand2, updateFlags) =>
          node.defs += reg1
          node.uses += reg2
          checkIfOperandUsed(node, operand2)
          node.succs += getCFGNode(nodeId + 1)
        case SmullInstr(reg1, reg2, reg3, reg4) =>
          node.defs += reg1
          node.defs += reg2
          node.uses += reg3
          node.uses += reg4
          node.succs += getCFGNode(nodeId + 1)
        case CmpInstr(reg, operand) =>
          node.uses += reg
          checkIfOperandUsed(node, operand)
          node.succs += getCFGNode(nodeId + 1)
        case Tst(reg, operand) =>
          node.uses += reg
          checkIfOperandUsed(node, operand)
          node.succs += getCFGNode(nodeId + 1)
        case BInstr(label, condition, storeReturnAddr) =>
          node.succs += getCFGNode(nodeId + 1)
          labelToNode.get(label) match {
            case Some(n) => node.succs += n
            case None => addLabelReference(label, node)
          }
        case BicInstr(reg1, reg2, operand) =>
          node.defs += reg1
          node.uses += reg2
          checkIfOperandUsed(node, operand)
          node.succs += getCFGNode(nodeId + 1)
        case AscizInstr(label: String, operand: AscizOperand) =>
          addLabelToNode(label, node)
        case StrInstr(reg, operand, size) =>
          node.uses += reg
          checkIfOperandUsed(node, operand)
          node.succs += getCFGNode(nodeId + 1)
        case RsbsInstr(reg1, reg2, operand) =>
          node.defs += reg1
          node.uses += reg2
          checkIfOperandUsed(node, operand)
          node.succs += getCFGNode(nodeId + 1)
        case NewLine() => 
          getCFGNode(nodeId - 1).succs -= getCFGNode(nodeId)
      }

      nodeId += 1
    }

    println(cfgNodes.mkString("\n"))
  }

  def getCFGNode(id: Int): CFGNode = {
    val newNode = cfgNodes.find(_.id == id)
    newNode match {
      case Some(n) => n
      case None =>
        val n = new CFGNode(id)
        cfgNodes += n
        n
    }
  }

  def makeInstructions(): ListBuffer[Instruction] = {
    val instrs = ListBuffer[Instruction]()
    for (node <- cfgNodes) {
      node.instr match {
        case Some(i) => instrs += i
        case None =>
      }
    }
    instrs
  }
}
