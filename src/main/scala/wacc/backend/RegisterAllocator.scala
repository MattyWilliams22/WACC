package wacc.backend

import wacc.backend.Instructions.{Push, Pop, AssemblyLine}

sealed trait RegisterAllocator {
  def allocateRegister(): Register
}

/* Basic register allocator that allocates registers by always putting the result in the register
   that is at the front of the list of available registers */
class BasicRegisterAllocator extends RegisterAllocator {
  private val allRegisters: List[Register] = List(RAX, RBX, RCX, RDX, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15)

  private var availableRegisters: List[Register] = allRegisters

  def allocateRegister(): Register = {
    val result = availableRegisters.head
    println(s"Allocated register: $result")

    /* Remove the allocated register from the list of available registers */
    availableRegisters = availableRegisters.tail
    result
  }

  /* Push all registers that are currently being used onto the stack */
  def saveRegisters(): List[AssemblyLine] = {
    println("Saving registers")
    (allRegisters diff availableRegisters).map(reg => Push(reg))
  }

  /* Pop all registers that were saved onto the stack from the stack */
  def restoreRegisters(): List[AssemblyLine] = {
    println("Restoring registers")
    (allRegisters diff availableRegisters).reverse.map(reg => Pop(reg))
  }

  def getAllRegisters: List[Register] = allRegisters
}