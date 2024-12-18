package wacc.backend

sealed trait Instruction

case class Comment(comment: String, indent: Int) extends Instruction
case class Command(str: String, indent: Int) extends Instruction
case class Label(name: String) extends Instruction
case class Push(regs: List[Register]) extends Instruction
case class Pop(regs: List[Register]) extends Instruction
case class Ldr(reg: Register, operand: LdrOperand) extends Instruction
case class AdrInstr(reg: Register, label: String) extends Instruction
case class Mov(reg: Register, operand: GeneralOperand, condition: Condition = noCondition) extends Instruction
case class AddInstr(reg1: Register, reg2: Register, operand2: GeneralOperand, updateFlags: Boolean = false) extends Instruction
case class SubInstr(reg: Register, reg2: Register, operand2: GeneralOperand, updateFlags: Boolean = false) extends Instruction
case class SmullInstr(reg1: Register, reg2: Register, reg3: Register, reg4: Register) extends Instruction
case class CmpInstr(reg: Register, operand: GeneralOperand) extends Instruction
case class Tst(reg: Register, operand: GeneralOperand) extends Instruction
case class BInstr(label: String, condition: Condition = noCondition, storeReturnAddr: Boolean = false) extends Instruction
case class BicInstr(reg1: Register, reg2: Register, operand: GeneralOperand) extends Instruction
case class AscizInstr(label: String, operand: AscizOperand) extends Instruction
case class StrInstr(reg: Register, operand: Addr, size: ElemSize = FourBytes) extends Instruction
case class RsbsInstr(reg1: Register, reg2: Register, operand: GeneralOperand) extends Instruction
case class NewLine() extends Instruction
