package wacc.backend

// Intel syntax arm32 instructions

object Instructions {

  trait Operand {
    def format: String
  }

  case class ImmVal(value: Int) extends Operand {
    override def format: String = "#" + value.toString
  }

  case class ShiftLeft(n: Int) extends Operand {
    override def format: String = "lsl #" + n.toString
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
  
  case class Push(reg: Register) extends AssemblyLine {
    override def format: String = s"    push {${reg.format}}"
  }

  case class PushMultiple(regs: List[Register]) extends AssemblyLine {
    override def format: String = s"    push {${regs.map(_.format).mkString(", ")}}"
  }

  case class Pop(reg: Register) extends AssemblyLine {
    override def format: String = s"    pop {${reg.format}}"
  }

  case class PopMultiple(regs: List[Register]) extends AssemblyLine {
    override def format: String = s"    pop {${regs.map(_.format).mkString(", ")}}"
  }

  case class LdrAddr(reg: Register, address: Register, offset: ImmVal) extends AssemblyLine {
    override def format: String = s"    ldr ${reg.format}, [${address.format}, ${offset.format}]"
  }

  case class LdrLabel(reg: Register, label: LabelAddr) extends AssemblyLine {
    override def format: String = s"    ldr ${reg.format}, ${label.format}"
  }

  case class LdrShift(reg1: Register, reg2: Register, reg3: Register, shift: ShiftLeft) extends AssemblyLine {
    override def format: String = s"    ldr ${reg1.format}, [${reg2.format}, ${reg3.format}, ${shift.format}]"
  }

  case class AdrInstr(reg: Register, label: String) extends AssemblyLine {
    override def format: String = s"    adr ${reg.format}, $label"
  }

  case class Mov(reg: Register, operand: Operand) extends AssemblyLine {
    override def format: String = s"    mov ${reg.format}, ${operand.format}"
  }

  case class Movge(reg: Register, operand: Operand) extends AssemblyLine {
    override def format: String = s"    movge ${reg.format}, ${operand.format}"
  }

  case class Movlt(reg: Register, operand: Operand) extends AssemblyLine {
    override def format: String = s"    movlt ${reg.format}, ${operand.format}"
  }

  case class Moveq(reg: Register, operand: Operand) extends AssemblyLine {
    override def format: String = s"    moveq ${reg.format}, ${operand.format}"
  }

  case class Movne(reg: Register, operand: Operand) extends AssemblyLine {
    override def format: String = s"    movne ${reg.format}, ${operand.format}"
  }

  case class Movgt(reg: Register, operand: Operand) extends AssemblyLine {
    override def format: String = s"    movgt ${reg.format}, ${operand.format}"
  }

  case class Movle(reg: Register, operand: Operand) extends AssemblyLine {
    override def format: String = s"    movle ${reg.format}, ${operand.format}"
  }

  case class AddInstr(reg: Register, operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    add ${reg.format}, ${operand1.format}, ${operand2.format}"
  }

  case class SubInstr(reg: Register, operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    sub ${reg.format}, ${operand1.format}, ${operand2.format}"
  }
  
  case class MulInstr(reg: Register, operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    mul ${reg.format}, ${operand1.format}, ${operand2.format}"
  }

  case class DivInstr(reg: Register, operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    sdiv ${reg.format}, ${operand1.format}, ${operand2.format}"
  }

  case class CmpInstr(operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    cmp ${operand1.format}, ${operand2.format}"
  }

  case class BInstr(label: String) extends AssemblyLine {
    override def format: String = s"    b $label"
  }

  case class BlInstr(label: String) extends AssemblyLine {
    override def format: String = s"    bl $label"
  }

  case class BicInstr(reg: Register, operand1: Register, operand2: Operand) extends AssemblyLine {
    override def format: String = s"    bic ${reg.format}, ${operand1.format}, ${operand2.format}"
  }

  case class BneInstr(label: String) extends AssemblyLine {
    override def format: String = s"    bne $label"
  }

  case class BeqInstr(label: String) extends AssemblyLine {
    override def format: String = s"    beq $label"
  }

  case class BlltInstr(label: String) extends AssemblyLine {
    override def format: String = s"    bllt $label"
  }

  case class BlgeInstr(label: String) extends AssemblyLine {
    override def format: String = s"    blge $label"
  }

  case class AscizInstr(label: String, string: String) extends AssemblyLine {
    override def format: String = s"   .word ${string.length}\n$label:\n   .asciz \"$string\""
  }

  case class StoreInstr(reg: Register, address: Register, offset: ImmVal) extends AssemblyLine {
    override def format: String = s"    str ${reg.format}, [${address.format}, ${offset.format}]"
  }

  case class NewLine() extends AssemblyLine {
    override def format: String = s""
  }

}