package wacc.backend

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object RegisterMapping {
  val regMap: mutable.Map[Register, Register] = mutable.Map.empty

  def addRegisterMapping(from: Register, to: Register) = {
    regMap.put(from, to)
  }

  def getRegColour(reg: Register): Register = {
    regMap.getOrElse(reg, reg)
  }

  def getGeneralColour(op: GeneralOperand): GeneralOperand = {
    op match {
      case r: Register => getRegColour(r)
      case RegShift(r, s) => RegShift(getRegColour(r), s)
      case _ => op
    }
  }

  def getLdrColour(op: LdrOperand): LdrOperand = {
    op match {
      case a: Addr => getAddrColour(a)
      case _ => op
    }
  }

  def getAddrColour(op: Addr): Addr = {
    op match {
      case Addr(r, o) => Addr(getRegColour(r), getGeneralColour(o))
      case _ => op
    }
  }

  def replaceInstructions(instrs: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    instrs.map(replaceInstruction)
  }

  def replaceInstruction(instr: Instruction): Instruction = {
    instr match {
      case Push(regs) =>
        Push(regs.map(r => getRegColour(r)))
      case Pop(regs) =>
        Pop(regs.map(r => getRegColour(r)))
      case Ldr(reg, operand) =>
        Ldr(getRegColour(reg), getLdrColour(operand))
      case AdrInstr(reg, label) =>
        AdrInstr(getRegColour(reg), label)
      case Mov(reg, operand, condition) =>
        Mov(getRegColour(reg), getGeneralColour(operand), condition)
      case AddInstr(reg1, reg2, operand2, updateFlags) =>
        AddInstr(getRegColour(reg1), getRegColour(reg2), getGeneralColour(operand2), updateFlags)
      case SubInstr(reg1, reg2, operand2, updateFlags) =>
        SubInstr(getRegColour(reg1), getRegColour(reg2), getGeneralColour(operand2), updateFlags)
      case SmullInstr(reg1, reg2, reg3, reg4) =>
        SmullInstr(getRegColour(reg1), getRegColour(reg2), getRegColour(reg3), getRegColour(reg4))
      case CmpInstr(reg, operand) =>
        CmpInstr(getRegColour(reg), getGeneralColour(operand))
      case Tst(reg, operand) =>
        Tst(getRegColour(reg), getGeneralColour(operand))
      case BicInstr(reg1, reg2, operand) =>
        BicInstr(getRegColour(reg1), getRegColour(reg2), getGeneralColour(operand))
      case StrInstr(reg, operand, size) =>
        StrInstr(getRegColour(reg), getAddrColour(operand), size)
      case RsbsInstr(reg1, reg2, operand) =>
        RsbsInstr(getRegColour(reg1), getRegColour(reg2), getGeneralColour(operand))
      case _ => instr
    }
  }
}
