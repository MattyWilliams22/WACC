package wacc.backend

trait Operand

trait GeneralOperand extends Operand
case class ImmVal(value: Int) extends GeneralOperand
case class RegShift(reg: Register, shift: Shift) extends GeneralOperand

sealed trait LdrOperand extends Operand

case class Addr(address: Register, offset: GeneralOperand) extends LdrOperand
case class LabelAddr(label: String) extends LdrOperand
case class IntLiteral(value: Int) extends LdrOperand

sealed trait AscizOperand extends Operand
case class StringLiteral(string: String) extends AscizOperand
case class ErrorMessage(error: Error) extends AscizOperand

sealed trait Shift

private case object noShift extends Shift
case class ShiftLeft(n: Int) extends Shift
case class ShiftRight(n: Int) extends Shift

sealed trait ElemSize

case object OneByte extends ElemSize
case object FourBytes extends ElemSize
