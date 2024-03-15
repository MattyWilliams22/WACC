package wacc.backend

import java.io.PrintWriter

object ARMAssemblyPrinter {

  /* Function to convert a given list of assembly instructions to their ARM32 string representation */
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
      case RegShift(reg, shift) => s"${formatReg(reg)}${formatShift(shift)}"
      case IntLiteral(num) => s"=${num.toString}"
      case StringLiteral(str) => str
      case ErrorMessage(error) => "Error: " + (error match {
        case OutOfMemoryErr => "Out of memory"
        case IndexOutOfBoundsErr => "Array index out of bounds"
        case NullReferenceErr => "Null pair de-referenced"
        case IntegerOverflowUnderflowErr => "Integer overflow or underflow occurred"
        case CharNotInRangeErr => "int %d is not ascii character 0-127"
        case DivByZeroErr => "Division by zero"
      })
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
      case T(n) => s"r${n.toString}"
    }

    def formatInstr(instr: Instruction): String = instr match {
      case Comment(comment, indent) =>
        " " * indent + s"@ $comment"
      case Command(str, indent) =>
        " " * indent + "." + str
      case Label(name) =>
        name + ":"
      case Push(regs) =>
        s"    push {${regs.map(formatReg).mkString(", ")}}"
      case Pop(regs) =>
        s"    pop {${regs.map(formatReg).mkString(", ")}}"
      case Ldr(reg, operand) =>
        s"    ldr ${formatReg(reg)}, ${formatOperand(operand)}"
      case AdrInstr(reg, label) =>
        s"    adr ${formatReg(reg)}, $label"
      case Mov(reg, operand, condition) =>
        s"    mov${formatCondition(condition)} ${formatReg(reg)}, ${formatOperand(operand)}"
      case AddInstr(reg, operand1, operand2, updateFlags) =>
        val updateFlagsString = if (updateFlags) "s" else ""

        s"    add$updateFlagsString ${formatReg(reg)}, ${formatOperand(operand1)}, ${formatOperand(operand2)}"
      case SubInstr(reg, operand1, operand2, updateFlags) =>
        val updateFlagsString = if (updateFlags) "s" else ""

        s"    sub$updateFlagsString ${formatReg(reg)}, ${formatOperand(operand1)}, ${formatOperand(operand2)}"
      case SmullInstr(reg1, reg2, operand1, operand2) =>
        s"    smull ${formatReg(reg1)}, ${formatReg(reg2)}, ${formatOperand(operand1)}, ${formatOperand(operand2)}"
      case CmpInstr(operand1, operand2) =>
        s"    cmp ${formatOperand(operand1)}, ${formatOperand(operand2)}"
      case Tst(operand1, operand2) =>
        s"    tst ${formatOperand(operand1)}, ${formatOperand(operand2)}"
      case BInstr(label, condition, storeReturnAddr) =>
        val storeReturnAddrString = if (storeReturnAddr) "l" else ""

        s"    b$storeReturnAddrString${formatCondition(condition)} $label"
      case BicInstr(reg, operand1, operand2) =>
        s"    bic ${formatReg(reg)}, ${formatOperand(operand1)}, ${formatOperand(operand2)}"
      case AscizInstr(label, operand) =>
        val escapedString = formatOperand(operand)
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
      case StrInstr(reg1, operand, size) =>
        s"    str${formatSize(size)} ${formatReg(reg1)}, ${formatOperand(operand)}"
      case RsbsInstr(reg, operand1, operand2) =>
        s"    rsbs ${formatReg(reg)}, ${formatOperand(operand1)}, ${formatOperand(operand2)}"
      case NewLine() => ""
    }

    def writeInstr(instr: Instruction): Unit = {
      val formattedInstr = formatInstr(instr) + "\n"
      writer.write(formattedInstr)
    }

    instructions.foreach{writeInstr}
  }
}
