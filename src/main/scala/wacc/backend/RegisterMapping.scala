package wacc.backend

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class RegisterMapping(val regMap: Map[Register, RegisterLocation]) {

  private def getRegColour(reg: Register): (Register, ListBuffer[Instruction], ListBuffer[Instruction]) = {
    regMap(reg) match {
      case RegisterLocation(FP, -1) => (FP, ListBuffer(), ListBuffer())
      case RegisterLocation(FP, offset) => 
        (
          IP, 
          ListBuffer(Push(List(IP)), Ldr(IP, Addr(FP, ImmVal(offset)))), 
          ListBuffer(StrInstr(IP, Addr(FP, ImmVal(offset))), Pop(List(IP)))
        )
      case RegisterLocation(r, _) => (r, ListBuffer(), ListBuffer())
    }
  }

  private def getGeneralColour(op: GeneralOperand): (GeneralOperand, ListBuffer[Instruction], ListBuffer[Instruction]) = {
    op match {
      case r: Register => getRegColour(r)
      case RegShift(r, s) => 
        val (reg, before, after) = getRegColour(r)
        (RegShift(reg, s), before, after)
      case _ => (op, ListBuffer(), ListBuffer())
    }
  }

  private def getLdrColour(op: LdrOperand): (LdrOperand, ListBuffer[Instruction], ListBuffer[Instruction]) = {
    op match {
      case a: Addr => getAddrColour(a)
      case _ => (op, ListBuffer(), ListBuffer())
    }
  }

  private def getAddrColour(op: Addr): (Addr, ListBuffer[Instruction], ListBuffer[Instruction]) = {
    op match {
      case Addr(r, o) => 
        val (reg, before1, after1) = getRegColour(r)
        val (genOp, before2, after2) = getGeneralColour(o)
        (Addr(reg, genOp), before1 ++ before2, after1 ++ after2)
      case _ => (op, ListBuffer(), ListBuffer())
    }
  }

  def replaceInstructions(instrs: ListBuffer[Instruction]): ListBuffer[Instruction] = {
    var newInstrs: ListBuffer[Instruction] = ListBuffer[Instruction]()
    for (instr <- instrs) {
      newInstrs ++= replaceInstruction(instr)
    }
    newInstrs
  }

  private def replaceInstruction(instr: Instruction): ListBuffer[Instruction] = {
    instr match {
      case Push(regs) =>
        val pushLines: ListBuffer[Instruction] = ListBuffer()
        val (regsList, before, after) = regs.foldLeft((ListBuffer[Register](), ListBuffer[Instruction](), ListBuffer[Instruction]())) {
          case ((regsList, before, after), r) =>
            val (reg, before1, after1) = getRegColour(r)
            (regsList += reg, before ++ before1, after ++ after1)
        }
        pushLines ++= before
        pushLines += Push(regsList.toList)
        pushLines ++= after
        pushLines
      case Pop(regs) =>
        val popLines: ListBuffer[Instruction] = ListBuffer()
        val (regsList, before, after) = regs.foldLeft((ListBuffer[Register](), ListBuffer[Instruction](), ListBuffer[Instruction]())) {
          case ((regsList, before, after), r) =>
            val (reg, before1, after1) = getRegColour(r)
            (regsList += reg, before ++ before1, after ++ after1)
        }
        popLines ++= before
        popLines += Pop(regsList.toList)
        popLines ++= after
        popLines
      case Ldr(reg, operand) =>
        val ldrLines: ListBuffer[Instruction] = ListBuffer()
        val (r, before1, after1) = getRegColour(reg)
        val (op, before2, after2) = getLdrColour(operand)
        ldrLines ++= before1
        ldrLines ++= before2
        ldrLines += Ldr(r, op)
        ldrLines ++= after2
        ldrLines ++= after1
        ldrLines
      case AdrInstr(reg, label) =>
        val (r, before, after) = getRegColour(reg)
        before ++ ListBuffer(AdrInstr(r, label)) ++ after
      case Mov(reg, operand, condition) =>
        val movLines: ListBuffer[Instruction] = ListBuffer()
        val (r1, before1, after1) = getRegColour(reg)
        val (op, before2, after2) = getGeneralColour(operand)
        movLines ++= before1
        movLines ++= before2
        movLines += Mov(r1, op, condition)
        movLines ++= after2
        movLines ++= after1
        movLines
      case AddInstr(reg1, reg2, operand2, updateFlags) =>
        val adrLines: ListBuffer[Instruction] = ListBuffer()
        val (r1, before1, after1) = getRegColour(reg1)
        val (r2, before2, after2) = getRegColour(reg2)
        val (op, before3, after3) = getGeneralColour(operand2)
        adrLines ++= before1
        adrLines ++= before2
        adrLines ++= before3
        adrLines += AddInstr(r1, r2, op, updateFlags)
        adrLines ++= after3
        adrLines ++= after2
        adrLines ++= after1
        adrLines
      case SubInstr(reg1, reg2, operand2, updateFlags) =>
        val subLines: ListBuffer[Instruction] = ListBuffer()
        val (r1, before1, after1) = getRegColour(reg1)
        val (r2, before2, after2) = getRegColour(reg2)
        val (op, before3, after3) = getGeneralColour(operand2)
        subLines ++= before1
        subLines ++= before2
        subLines ++= before3
        subLines += SubInstr(r1, r2, op, updateFlags)
        subLines ++= after3
        subLines ++= after2
        subLines ++= after1
        subLines
      case SmullInstr(reg1, reg2, reg3, reg4) =>
        val smullLines: ListBuffer[Instruction] = ListBuffer()
        val (r1, before1, after1) = getRegColour(reg1)
        val (r2, before2, after2) = getRegColour(reg2)
        val (r3, before3, after3) = getRegColour(reg3)
        val (r4, before4, after4) = getRegColour(reg4)
        smullLines ++= before1
        smullLines ++= before2
        smullLines ++= before3
        smullLines ++= before4  
        smullLines += SmullInstr(r1, r2, r3, r4)
        smullLines ++= after4
        smullLines ++= after3
        smullLines ++= after2
        smullLines ++= after1
        smullLines
      case CmpInstr(reg, operand) =>
        val cmpLines: ListBuffer[Instruction] = ListBuffer()
        val (r, before1, after1) = getRegColour(reg)
        val (op, before2, after2) = getGeneralColour(operand)
        cmpLines ++= before1
        cmpLines ++= before2
        cmpLines += CmpInstr(r, op)
        cmpLines ++= after2
        cmpLines ++= after1
        cmpLines
      case Tst(reg, operand) =>
        val tstLines: ListBuffer[Instruction] = ListBuffer()
        val (r, before1, after1) = getRegColour(reg)
        val (op, before2, after2) = getGeneralColour(operand)
        tstLines ++= before1
        tstLines ++= before2
        tstLines += Tst(r, op)
        tstLines ++= after2
        tstLines ++= after1
        tstLines
      case BicInstr(reg1, reg2, operand) =>
        val bicLines: ListBuffer[Instruction] = ListBuffer()
        val (r1, before1, after1) = getRegColour(reg1)
        val (r2, before2, after2) = getRegColour(reg2)
        val (op, before3, after3) = getGeneralColour(operand)
        bicLines ++= before1
        bicLines ++= before2
        bicLines ++= before3
        bicLines += BicInstr(r1, r2, op)
        bicLines ++= after3
        bicLines ++= after2
        bicLines ++= after1
        bicLines
      case StrInstr(reg, operand, size) =>
        val strLines: ListBuffer[Instruction] = ListBuffer()
        val (r, before1, after1) = getRegColour(reg)
        val (op, before2, after2) = getAddrColour(operand)
        strLines ++= before1
        strLines ++= before2
        strLines += StrInstr(r, op, size)
        strLines ++= after2
        strLines ++= after1
        strLines
      case RsbsInstr(reg1, reg2, operand) =>
        val rsbsLines: ListBuffer[Instruction] = ListBuffer()
        val (r1, before1, after1) = getRegColour(reg1)
        val (r2, before2, after2) = getRegColour(reg2)
        val (op, before3, after3) = getGeneralColour(operand)
        rsbsLines ++= before1
        rsbsLines ++= before2 
        rsbsLines ++= before3
        rsbsLines += RsbsInstr(r1, r2, op)
        rsbsLines ++= after3
        rsbsLines ++= after2
        rsbsLines ++= after1
        rsbsLines
      case _ => ListBuffer(instr)
    }
  }
}
