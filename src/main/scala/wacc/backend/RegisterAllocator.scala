package wacc.backend

sealed trait RegisterAllocator {
  def allocateRegister(): Register
}

/* Basic register allocator that allocates registers by always putting the result in the register
   that is at the front of the list of available registers */
class BasicRegisterAllocator extends RegisterAllocator {
  private val allRegisters: List[Register] = List(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15)

  private var availableRegisters: List[Register] = allRegisters

  def allocateRegister(): Register = {
    val result = availableRegisters.head
    println(s"Allocated register: $result")

    /* Remove the allocated register from the list of available registers */
    availableRegisters = availableRegisters.tail
    result
  }

  /* Push all registers that are currently being used onto the stack */
  def saveRegisters(): Unit = {
    println("Saving registers")
  }

  /* Pop all registers that were saved onto the stack from the stack */
  def restoreRegisters(): Unit = {
    println("Restoring registers")
  }
}