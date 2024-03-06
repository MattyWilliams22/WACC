package wacc.backend

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import wacc.backend.RegisterMapping._

object GraphColouring {

  def mapRegister(reg: Register): Unit = {
    addRegisterMapping(reg, reg)
  }

  def mapGeneralOp(op: GeneralOperand): Unit = {
    op match {
      case r: Register => mapRegister(r)
      case RegShift(r, s) => mapRegister(r)
      case _ =>
    }
  }

  def mapLdrOp(op: LdrOperand): Unit = {
    op match {
      case Addr(r, o) => 
        mapRegister(r)
        mapGeneralOp(o)
      case _ => 
    }
  }

  def mapInstructions(instrs: ListBuffer[Instruction]): Unit = {
    instrs.map(mapInstruction)
  }

  def mapInstruction(instr: Instruction): Unit = {
    instr match {
      case Push(regs) =>
        regs.map(r => mapRegister(r))
      case Pop(regs) =>
        regs.map(r => mapRegister(r))
      case Ldr(reg, operand) =>
        mapRegister(reg)
        mapLdrOp(operand)
      case AdrInstr(reg, label) =>
        mapRegister(reg)
      case Mov(reg, operand, condition) =>
        mapRegister(reg)
        mapGeneralOp(operand)
      case AddInstr(reg1, reg2, operand2, updateFlags) =>
        mapRegister(reg1)
        mapRegister(reg2)
        mapGeneralOp(operand2)
      case SubInstr(reg1, reg2, operand2, updateFlags) =>
        mapRegister(reg1)
        mapRegister(reg2)
        mapGeneralOp(operand2)
      case SmullInstr(reg1, reg2, reg3, reg4) =>
        mapRegister(reg1)
        mapRegister(reg2)
        mapRegister(reg3)
        mapRegister(reg4)
      case CmpInstr(reg, operand) =>
        mapRegister(reg)
        mapGeneralOp(operand)
      case Tst(reg, operand) =>
        mapRegister(reg)
        mapGeneralOp(operand)
      case BicInstr(reg1, reg2, operand) =>
        mapRegister(reg1)
        mapRegister(reg2)
        mapGeneralOp(operand)
      case StrInstr(reg, operand, size) =>
        mapRegister(reg)
        mapLdrOp(operand)
      case RsbsInstr(reg1, reg2, operand) =>
        mapRegister(reg1)
        mapRegister(reg2)
        mapGeneralOp(operand)
      case _ =>
    }
  }
}
