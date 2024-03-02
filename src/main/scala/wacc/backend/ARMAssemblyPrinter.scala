package wacc.backend

import java.io.PrintWriter

object ARMAssemblyPrinter {

  /* Function to convert a given list of ARM32 assembly instructions to its string representation */
  def printAssembly(instructions: List[Instruction], writer: PrintWriter): Unit = {

    def formatSize(size: ElemSize): String = size match {
      case OneByte => "b"
      case _ => ""
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
      case ImmVal(value) => "#" + value.toString
      case LabelAddr(label) => "=" + label
      case reg: Register => formatReg(reg)
      case Addr(address, offset) => s"[${formatReg(address)}, ${formatOperand(offset)}]"
      case RegShift(reg1, reg2, shift) => s"[${formatReg(reg1)}, ${formatReg(reg2)}${formatShift(shift)}]"
      case IntLiteral(num) => s"=${num.toString}"
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

    def formatInstr(instr: Instruction): String = instr match {
      case Comment(comment) => s"@ $comment"
      case Command(str, indent) => " " * indent + "." + str
      case Label(name) => name + ":"
      case Push(regs) => s"    push {${regs.map(formatReg).mkString(", ")}}"
      case Pop(regs) => s"    pop {${regs.map(formatReg).mkString(", ")}}"
      case Ldr(reg, operand) => s"    ldr ${formatReg(reg)}, ${formatOperand(operand)}"
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
      case StrInstr(reg1, operand, size) => s"    str${formatSize(size)} ${formatReg(reg1)}, ${formatOperand(operand)}"
      case RsbsInstr(reg, operand) => s"    rsbs ${formatReg(reg)}, ${formatOperand(operand)}, #0"
      case NewLine() => ""
    }

    def writeInstr(instr: Instruction): Unit = {
      val formattedInstr = formatInstr(instr) + "\n"
      writer.write(formattedInstr)
    }

    instructions.foreach{writeInstr}
  }
}
