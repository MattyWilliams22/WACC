package wacc.backend

// Intel syntax arm32 instructions

object Instructions {

  trait Operand {
    def format: String
  }

  case class ImmVal(value: Int) extends Operand {
    override def format: String = "#" + value.toString
  }

  sealed trait AssemblyLine {
    def format: String
  }

  case class Comment(comment: String) extends AssemblyLine {
    override def format: String = s"@ $comment"
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

  case class Ldr(reg: Register, address: Register) extends AssemblyLine {
    override def format: String = s"    ldr ${reg.format}, [${address.format}]"
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

  case class AscizInstr(label: String, string: String) extends AssemblyLine {
    override def format: String = s"$label:\n   .asciz $string"
  }

  case class RetInstr() extends AssemblyLine {
    // Must check this works correctly with BLInstr
    override def format: String = s"    mov ${LR.format}, ${PC.format}"
  }

}