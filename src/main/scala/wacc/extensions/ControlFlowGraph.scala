package wacc.extensions

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import wacc.backend._

import scala.annotation.tailrec

class ControlFlowGraph {
  private object CFGNodeOrdering extends Ordering[CFGNode] {
    def compare(a: CFGNode, b: CFGNode): Int = a.id compare b.id
  }

  val cfgNodes: mutable.Set[CFGNode] = mutable.SortedSet[CFGNode]()(CFGNodeOrdering)
  var labelToNode: mutable.Map[String, CFGNode] = mutable.Map[String, CFGNode]()
  var startNode: Option[CFGNode] = None
  var fileRanges: List[(Int, Int)] = List[(Int, Int)]()

  private val labelReferences: mutable.Map[String, ListBuffer[CFGNode]] = mutable.Map[String, ListBuffer[CFGNode]]()

  private def addLabelToNode(label: String, node: CFGNode): Unit = {
    labelToNode += (label -> node)
    if (labelReferences.contains(label)) {
      labelReferences(label).map(n => n.succs += node)
    }
  }

  private def addLabelReference(label: String, node: CFGNode): Unit = {
    if (labelReferences.contains(label)) {
      labelReferences(label) += node
    } else {
      labelReferences += (label -> ListBuffer(node))
    }
  }

  @tailrec
  private def checkIfOperandUsed(node: CFGNode, op: Operand): Unit = {
    op match {
      case r: Register => node.uses += r
      case RegShift(r, _) => node.uses += r
      case Addr(r, o) => 
        node.uses += r
        checkIfOperandUsed(node, o)
      case _ =>
    }
  }

  def printCFG(): Unit = {
    for (cfgNode <- cfgNodes) {
      println("Node " + cfgNode.id + ": " + cfgNode.instr.getOrElse("No instruction"))
      println("Uses: " + cfgNode.uses.mkString(", "))
      println("Defs: " + cfgNode.defs.mkString(", "))
      println("-> " + cfgNode.succs.map(_.id).mkString(", "))
    }
  }

  private def buildBinOpNode(node: CFGNode, reg1: Register, reg2: Register, operand: GeneralOperand, nodeId: Int): Unit = {
    node.defs += reg1
    node.uses += reg2
    checkIfOperandUsed(node, operand)
    node.succs += getCFGNode(nodeId + 1)
  }

  def addToCFG(instrs: ListBuffer[Instruction]): Unit = {
    var nodeId = 0
    if (fileRanges.nonEmpty) {
      nodeId = fileRanges.last._2 + 1
    }
    val startId = nodeId

    for (instr <- instrs) {
      val node = getCFGNode(nodeId)
      node.instr = Some(instr)
      
      instr match {
        case Comment(_, _) =>
          nodeId -= 1
        case Command(str, _) => 
          str match {
            case "align 4" =>
              startNode match {
                case None => startNode = Some(node)
                case _ =>
              }
              node.succs += getCFGNode(nodeId + 1)
            case "ltorg" =>
            case _ => 
              node.succs += getCFGNode(nodeId + 1)
          }
        case Label(name) =>
          addLabelToNode(name, node)
          node.succs += getCFGNode(nodeId + 1)
          node.succs += getCFGNode(nodeId - 1)
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
          operand match {
            case LabelAddr(string) =>
              labelToNode.get(string) match {
                case Some(n) => node.succs += n
                case None => addLabelReference(string, node)
              }
            case _ =>
          }
        case AdrInstr(reg, string) =>
          node.defs += reg
          node.succs += getCFGNode(nodeId + 1)
          labelToNode.get(string) match {
            case Some(n) => node.succs += n
            case None => addLabelReference(string, node)
          }
        case Mov(reg, operand, _) =>
          node.defs += reg
          checkIfOperandUsed(node, operand)
          node.succs += getCFGNode(nodeId + 1)
        case AddInstr(reg1, reg2, operand2, _) =>
          buildBinOpNode(node, reg1, reg2, operand2, nodeId)
        case SubInstr(reg1, reg2, operand2, _) =>
          buildBinOpNode(node, reg1, reg2, operand2, nodeId)
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
        case BInstr(label, _, _) =>
          node.succs += getCFGNode(nodeId + 1)
          labelToNode.get(label) match {
            case Some(n) => node.succs += n
            case None => addLabelReference(label, node)
          }
        case BicInstr(reg1, reg2, operand) =>
          buildBinOpNode(node, reg1, reg2, operand, nodeId)
        case AscizInstr(string, _) => 
          addLabelToNode(string, node)
        case StrInstr(reg, operand, _) =>
          node.uses += reg
          checkIfOperandUsed(node, operand)
          node.succs += getCFGNode(nodeId + 1)
        case RsbsInstr(reg1, reg2, operand) =>
          buildBinOpNode(node, reg1, reg2, operand, nodeId)
        case NewLine() => 
          getCFGNode(nodeId - 1).succs -= getCFGNode(nodeId)
      }

      nodeId += 1
    }
    getCFGNode(nodeId - 1).succs.clear()
    val newRange = (startId, nodeId - 1)
    fileRanges = fileRanges :+ newRange
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
    val startId: Int = fileRanges.head._1
    val endId: Int = fileRanges.head._2
    fileRanges = fileRanges.tail
    val instrs = ListBuffer[Instruction]()
    for (node <- cfgNodes) {
      if (node.id >= startId && node.id <= endId) {
        node.instr match {
          case Some(i) => instrs += i
          case None =>
        }
      }
    }
    instrs
  }
}
