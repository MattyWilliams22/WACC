package wacc.backend

sealed trait Condition

case object noCondition extends Condition
case object EQcond extends Condition
case object NEcond extends Condition
case object GEcond extends Condition
case object LTcond extends Condition
case object GTcond extends Condition
case object LEcond extends Condition
case object VScond extends Condition
