package wacc.backend

/* Represents a register in the ARM32 architecture */
sealed trait Register extends GeneralOperand {
  val number: Int
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

/* Frame Pointer */
case object FP extends Register {
  val number: Int = 11
}

/* Inter-procedural Scratch */
case object IP extends Register {
  val number: Int = 12
}

/* Stack Pointer */
case object SP extends Register {
  val number: Int = 13
}

/* Link Register */
case object LR extends Register {
  val number: Int = 14
}

/* Program Counter */
case object PC extends Register {
  val number: Int = 15
}
