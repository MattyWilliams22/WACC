package wacc.backend

import wacc.backend.Instructions._

package object ARMAssemblyPrinter {

  def printAssembly(instructions: List[AssemblyLine]): String = {

    def formatSize(size: ElemSize): String = size match {
      case OneByte => "b"
      case FourBytes => ""
    }

    def formatCondition(condition: Condition): String = condition match {
      case EQcond => "eq"
      case NEcond => "ne"
      case GEcond => "ge"
      case LTcond => "lt"
      case GTcond => "gt"
      case LEcond => "le"
      case VScond => "vs"
      case _ => ""
    }

    def formatOperand(operand: Operand): String = operand match {
      case immVal: ImmVal => "#" + immVal.value.toString
      case labelAddr: LabelAddr => "=" + labelAddr.label
      case reg: Register => formatReg(reg)
    }

    def formatShift(shift: Shift): String = shift match {
      case ShiftLeft(n) => s", lsl #$n"
      case ShiftRight(n) => s", lsr #$n"
      case _ => ""
    }

    def formatReg(reg: Register): String = reg match {
      case R0 => "r0"
      case R1 => "r1"
      case R2 => "r2"
      case R3 => "r3"
      case R4 => "r4"
      case R5 => "r5"
      case R6 => "r6"
      case R7 => "r7"
      case R8 => "r8"
      case R9 => "r9"
      case R10 => "r10"
      case FP => "fp"
      case SP => "sp"
      case LR => "lr"
      case PC => "pc"
      case IP => "ip"
    }

    def formatInstr(instr: AssemblyLine): String = instr match {
      case Comment(comment) => s"@ $comment"
      case Command(str, indent) => " " * indent + "." + str
      case Label(name) => name + ":"
      case Push(regs) => s"    push {${regs.map(formatReg).mkString(", ")}}"
      case Pop(regs) => s"    pop {${regs.map(formatReg).mkString(", ")}}"
      case LdrImm(reg, num) => s"    ldr ${formatReg(reg)}, =${num.toString}"
      case LdrAddr(reg, addr, offset) => s"    ldr ${formatReg(reg)}, [${formatReg(addr)}, ${formatOperand(offset)}]"
      case LdrLabel(reg, labelAddr) => s"    ldr ${formatReg(reg)}, ${formatOperand(labelAddr)}"
      case LdrShift(reg1, reg2, reg3, shift) => s"    ldr ${formatReg(reg1)}, [${formatReg(reg2)}, ${formatReg(reg3)}${formatShift(shift)}]"
      case AdrInstr(reg, label) => s"    adr ${formatReg(reg)}, $label"
      case Mov(reg, operand, condition) => s"    mov${formatCondition(condition)} ${formatReg(reg)}, ${formatOperand(operand)}"
      case AddInstr(reg, operand1, operand2) => s"    add ${formatReg(reg)}, ${formatOperand(operand1)}, ${formatOperand(operand2)}"
      case AddsInstr(reg, operand1, operand2) => s"    adds ${formatReg(reg)}, ${formatOperand(operand1)}, ${formatOperand(operand2)}"
      case SubInstr(reg, operand1, operand2) => s"    sub ${formatReg(reg)}, ${formatOperand(operand1)}, ${formatOperand(operand2)}"
      case SubsInstr(reg, operand1, operand2) => s"    subs ${formatReg(reg)}, ${formatOperand(operand1)}, ${formatOperand(operand2)}"
      case SmullInstr(reg1, reg2, operand1, operand2) => s"    smull ${formatReg(reg1)}, ${formatReg(reg2)}, ${formatOperand(operand1)}, ${formatOperand(operand2)}"
      case CmpInstr(operand1, operand2, shift) => s"    cmp ${formatOperand(operand1)}, ${formatOperand(operand2)}${formatShift(shift)}"
      case Tst(operand1, operand2, shift) => s"    tst ${formatOperand(operand1)}, ${formatOperand(operand2)}${formatShift(shift)}"
      case BInstr(label, condition) => s"    b${formatCondition(condition)} $label"
      case BlInstr(label, condition) => s"    bl${formatCondition(condition)} $label"
      case BicInstr(reg, operand1, operand2) => s"    bic ${formatReg(reg)}, ${formatOperand(operand1)}, ${formatOperand(operand2)}"
      case AscizInstr(label, string) =>
        val escapedString = string
          .replace("\\", "\\\\")
          .replace("\"", "\\\"")
          .replace("\'", "\\\'")
          .replace("\b", "\\b")
          .replace("\n", "\\n")
          .replace("\r", "\\r")
          .replace("\t", "\\t")
          .replace("\f", "\\f")
          .replace("\u0000", "\\0")

        s"   .word ${escapedString.length}\n$label:\n   .asciz \"$escapedString\""
      case StoreInstr(reg, address, offset, size) => s"    str${formatSize(size)} ${formatReg(reg)}, [${formatReg(address)}, ${formatOperand(offset)}]"
      case StoreShift(reg1, reg2, reg3, shift) => s"    str ${formatReg(reg1)}, [${formatReg(reg2)}, ${formatReg(reg3)}${formatShift(shift)}]"
      case RsbsInstr(reg, operand) => s"    rsbs ${formatReg(reg)}, ${formatOperand(operand)}, #0"
      case NewLine() => ""
    }

    def appendInstr(stringBuilder: StringBuilder, instr: AssemblyLine): Unit = {
      val formattedInstr = formatInstr(instr)
      stringBuilder.append(formattedInstr).append("\n")
    }

    val stringBuilder = new StringBuilder()
    instructions.foreach { instr => appendInstr(stringBuilder, instr)}
    stringBuilder.toString()
  }
}
