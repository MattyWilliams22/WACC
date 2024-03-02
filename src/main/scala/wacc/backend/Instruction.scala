package wacc.backend

sealed trait Instruction

case class Comment(comment: String) extends Instruction
case class Command(str: String, indent: Int) extends Instruction
case class Label(name: String) extends Instruction
case class Push(regs: List[Register]) extends Instruction
case class Pop(regs: List[Register]) extends Instruction
case class Ldr(reg: Register, operand: Operand) extends Instruction
case class AdrInstr(reg: Register, label: String) extends Instruction
case class Mov(reg: Register, operand: Operand, condition: Condition = noCondition) extends Instruction
case class AddInstr(reg: Register, operand1: Register, operand2: Operand) extends Instruction
case class AddsInstr(reg: Register, operand1: Register, operand2: Operand) extends Instruction
case class SubInstr(reg: Register, operand1: Register, operand2: Operand) extends Instruction
case class SubsInstr(reg: Register, operand1: Register, operand2: Operand) extends Instruction
case class SmullInstr(reg1: Register, reg2: Register, operand1: Register, operand2: Operand) extends Instruction
case class CmpInstr(operand1: Register, operand2: Operand, shift: Shift = noShift) extends Instruction
case class Tst(operand1: Register, operand2: Operand, shift: Shift = noShift) extends Instruction
case class BInstr(label: String, condition: Condition = noCondition) extends Instruction
case class BlInstr(label: String, condition: Condition = noCondition) extends Instruction
case class BicInstr(reg: Register, operand1: Register, operand2: Operand) extends Instruction
case class AscizInstr(label: String, string: String) extends Instruction
case class StrInstr(reg: Register, operand: Operand, size: ElemSize = FourBytes) extends Instruction
case class RsbsInstr(reg: Register, operand: Operand) extends Instruction
case class NewLine() extends Instruction