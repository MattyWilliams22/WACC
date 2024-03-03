package wacc.backend

sealed trait Error

case object OutOfMemoryErr extends Error
case object IndexOutOfBoundsErr extends Error
case object NullReferenceErr extends Error
case object IntegerOverflowUnderflowErr extends Error
case object CharNotInRangeErr extends Error
case object DivByZeroErr extends Error