package wacc.backend

import wacc.backend.Instructions._

/* Represents a register in the Intel x86-64 architecture */
sealed trait Register extends Operand {
  val number: Int
  def format: String = s"r$number"
}

/* General Purpose Registers */
case object RAX extends Register {
  val number: Int = 0
}

case object RBX extends Register {
  val number: Int = 1
}

case object RCX extends Register {
  val number: Int = 2
}

case object RDX extends Register {
  val number: Int = 3
}

case object RSI extends Register {
  val number: Int = 4
}

case object RDI extends Register {
  val number: Int = 5
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
case object RSP extends Register {
  val number: Int = 6
  override def format: String = "rsp"
}

/* Base Pointer */
case object RBP extends Register {
  val number: Int = 7
  override def format: String = "rbp"
}
