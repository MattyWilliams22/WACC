package wacc.backend

// Intel syntax arm32 instructions

object Instructions {

  trait Operand {
    def format: String
  }

  sealed trait Shift extends Operand {
    def format: String
  } 

  case class ImmVal(value: Int) extends Operand {
    override def format: String = "#" + value.toString
  }

  case class ShiftLeft(n: Int) extends Operand with Shift {
    override def format: String = "lsl #" + n.toString
  }

  case class ShiftRight(n: Int) extends Operand with Shift {
    override def format: String = "lsr #" + n.toString
  }

  case class LabelAddr(label: String) extends Operand {
    override def format: String = "=" + label
  }

  sealed trait AssemblyLine {
    def format: String
  }

  case class Comment(comment: String) extends AssemblyLine {
    override def format: String = s"@ $comment"
  }

  case class DataSection() extends AssemblyLine {
    override def format: String = ".data"
  }

  case class Command(str: String) extends AssemblyLine {
    override def format: String = "." + str
  }

  case class Label(name: String) extends AssemblyLine {
    override def format: String = name + ":"
  }

  case class Push(regs: List[Register]) extends AssemblyLine {
    override def format: String = s"    push {${regs.map(_.format).mkString(", ")}}"
  }

  case class Pop(regs: List[Register]) extends AssemblyLine {
    override def format: String = s"    pop {${regs.map(_.format).mkString(", ")}}"
  }

  case class LdrAddr(reg: Register, address: Register, offset: ImmVal) extends AssemblyLine {
    override def format: String = s"    ldr ${reg.format}, [${address.format}, ${offset.format}]"
  }

  case class LdrLabel(reg: Register, label: LabelAddr) extends AssemblyLine {
    override def format: String = s"    ldr ${reg.format}, ${label.format}"
  }

  case class LdrShift(reg1: Register, reg2: Register, reg3: Register, shift: Shift) extends AssemblyLine {
    override def format: String = s"    ldr ${reg1.format}, [${reg2.format}, ${reg3.format}, ${shift.format}]"
  }

  case class AdrInstr(reg: Register, label: String) extends AssemblyLine {
    override def format: String = s"    adr ${reg.format}, $label"
  }

  case class Mov(reg: Register, operand: Operand, condition: Condition = noCondition) extends AssemblyLine {
    override def format: String = s"    mov${condition.format} ${reg.format}, ${operand.format}"
  }

  case class AddInstr(reg: Register, operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    add ${reg.format}, ${operand1.format}, ${operand2.format}"
  }

  case class AddsInstr(reg: Register, operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    adds ${reg.format}, ${operand1.format}, ${operand2.format}"
  }

  case class SubInstr(reg: Register, operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    sub ${reg.format}, ${operand1.format}, ${operand2.format}"
  }

  case class SubsInstr(reg: Register, operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    subs ${reg.format}, ${operand1.format}, ${operand2.format}"
  }
  
  case class MulInstr(reg: Register, operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    mul ${reg.format}, ${operand1.format}, ${operand2.format}"
  }

  case class SmullInstr(reg1: Register, reg2: Register, operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    smull ${reg1.format}, ${reg2.format}, ${operand1.format}, ${operand2.format}"
  }

  case class DivInstr(reg: Register, operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    sdiv ${reg.format}, ${operand1.format}, ${operand2.format}"
  }

  case class CmpInstr(operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    cmp ${operand1.format}, ${operand2.format}"
  }

  case class CmpShift(operand1: Register, operand2: Register, shift: Shift) extends AssemblyLine {
    override def format: String = s"    cmp ${operand1.format}, ${operand2.format}, ${shift.format}"
  }

  case class BInstr(label: String, condition: Condition = noCondition) extends AssemblyLine {
    override def format: String = s"    b${condition.format} $label"
  }

  case class BlInstr(label: String, condition: Condition = noCondition) extends AssemblyLine {
    override def format: String = s"    bl${condition.format} $label"
  }

  case class BicInstr(reg: Register, operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    bic ${reg.format}, ${operand1.format}, ${operand2.format}"
  }

  case class AscizInstr(label: String, string: String) extends AssemblyLine {
    override def format: String = s"   .word ${string.length}\n$label:\n   .asciz \"$string\""
  }

  case class StoreInstr(reg: Register, address: Register, offset: ImmVal) extends AssemblyLine {
    override def format: String = s"    str ${reg.format}, [${address.format}, ${offset.format}]"
  }

  case class StoreShift(reg1: Register, reg2: Register, reg3: Register, shift: Shift) extends AssemblyLine {
    override def format: String = s"    str ${reg1.format}, [${reg2.format}, ${reg3.format}, ${shift.format}]"
  }

  case class NewLine() extends AssemblyLine {
    override def format: String = s""
  }

  sealed trait Condition {
    def format: String
  }

  case object noCondition extends Condition {
    override def format: String = ""
  }

  case object EQcond extends Condition {
    override def format: String = "eq"
  }

  case object NEcond extends Condition {
    override def format: String = "ne"
  }

  case object GEcond extends Condition {
    override def format: String = "ge"
  }

  case object LTcond extends Condition {
    override def format: String = "lt"
  }

  case object GTcond extends Condition {
    override def format: String = "gt"
  }

  case object LEcond extends Condition {
    override def format: String = "le"
  }

}