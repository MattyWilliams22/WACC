package wacc.backend

trait Operand

case class ImmVal(value: Int) extends Operand
case class LabelAddr(label: String) extends Operand

sealed trait Shift

private case object noShift extends Shift
case class ShiftLeft(n: Int) extends Shift
case class ShiftRight(n: Int) extends Shift

sealed trait ElemSize

case object OneByte extends ElemSize
case object FourBytes extends ElemSize
