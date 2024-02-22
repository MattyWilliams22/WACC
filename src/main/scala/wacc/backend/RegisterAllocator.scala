package wacc.backend

import scala.collection.mutable

import wacc.backend.Instructions.{Push, Pop, AssemblyLine}

sealed trait RegisterAllocator {
  def allocateRegister(varName: Option[String]): Register
}

/* Basic register allocator that allocates registers by always putting the result in the register
   that is at the front of the list of available registers */
class BasicRegisterAllocator extends RegisterAllocator {
  private val allRegisters: List[Register] = List(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10)

  private var availableRegisters: List[Register] = List(R4, R5, R6, R7, R8, R9, R10)

  private var varMap: mutable.Map[String, Register] = mutable.Map.empty[String, Register]

  def allocateRegister(varName: Option[String]): Register = {
    val result = availableRegisters.head
    println(s"Allocated register: $result")

    if (varName.isDefined)
      changeRegister(varName.get, result)

    /* Remove the allocated register from the list of available registers */
    availableRegisters = availableRegisters.tail
    result
  }

  def lookupRegister(varName: String): Option[Register] = {
    varMap.get(varName)
  }

  def changeRegister(varName: String, register: Register): Unit = {
    if (varMap.contains(varName)) {
      val oldRegister = varMap(varName)
      availableRegisters = oldRegister :: availableRegisters.filterNot(_ == register)
    } else {
      availableRegisters = availableRegisters.filterNot(_ == register)
    }
    varMap(varName) = register
  }

  def removeVariable(varName: String): Unit = {
    availableRegisters = varMap(varName) :: availableRegisters
    varMap -= varName
  }

  def deallocateRegister(register: Register): Unit = {
    println(s"Deallocated register: $register")

    if (varMap.exists(_._2 == register)) {
      val varName = varMap.find(_._2 == register).get._1
      varMap -= varName
    }

    /* Add the deallocated register to the list of available registers */
    availableRegisters = register :: availableRegisters
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

  def getStartRegisters: List[Register] = List(R4, R5, R6, R7, R8, R9, R10)
}