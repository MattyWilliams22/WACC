package wacc.backend

trait Operand

case class IntLiteral(value: Int) extends Operand
case class ImmVal(value: Int) extends Operand
case class LabelAddr(label: String) extends Operand
case class Addr(address: Register, offset: Operand) extends Operand
case class RegShift(reg1: Register, reg2: Register, shift: Shift) extends Operand

sealed trait Shift

private case object noShift extends Shift
case class ShiftLeft(n: Int) extends Shift
case class ShiftRight(n: Int) extends Shift

sealed trait ElemSize

case object OneByte extends ElemSize
case object FourBytes extends ElemSize
