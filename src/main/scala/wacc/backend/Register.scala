package wacc.backend

import wacc.backend.Instructions._

/* Represents a register in the Intel x86-64 architecture */
sealed trait Register extends Operand {
  val number: Int
  def format: String = s"r$number"
}

/* General Purpose Registers */
case object R0 extends Register {
  val number: Int = 0
}

case object R1 extends Register {
  val number: Int = 1
}

case object R2 extends Register {
  val number: Int = 2
}

case object R3 extends Register {
  val number: Int = 3
}

case object R4 extends Register {
  val number: Int = 4
}

case object R5 extends Register {
  val number: Int = 5
}

case object R6 extends Register {
  val number: Int = 6
}

case object R7 extends Register {
  val number: Int = 7
}

case object R8 extends Register {
  val number: Int = 8
}

case object R9 extends Register {
  val number: Int = 9
}

case object R10 extends Register {
  val number: Int = 10
}

case object R11 extends Register {
  val number: Int = 11
}

case object R12 extends Register {
  val number: Int = 12
}

case object R13 extends Register {
  val number: Int = 13
}

case object R14 extends Register {
  val number: Int = 14
}

case object R15 extends Register {
  val number: Int = 15
}

/* Stack Pointer */
case object SP extends Register {
  val number: Int = 16
  override def format: String = "sp"
}

/* Frame Pointer */
case object FP extends Register {
  val number: Int = 17
  override def format: String = "fp"
}

/* Program Counter */
case object PC extends Register {
  val number: Int = 18
  override def format: String = "pc"
}
