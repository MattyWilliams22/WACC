package wacc.backend

import scala.collection.mutable

import wacc.ASTNodes._
import wacc.backend.Instructions._

case class VariableLocation(register: Register, offset: Int, size: Int, _type: Type)

sealed trait RegisterAllocator {
  def allocateRegister(): (Register, List[AssemblyLine])
}

/* Basic register allocator that allocates registers by always putting the result in the register
   that is at the front of the list of available registers */
class BasicRegisterAllocator extends RegisterAllocator {
  private val allRegisters: List[Register] = List(R4, R5, R6, R7, R8, R9, R10, R1, R2, R3)

  private var availableRegisters: List[Register] = allRegisters

  /* Map of variable names to their locations where they are stored */
  private val varMap: mutable.Map[String, VariableLocation] = mutable.Map.empty[String, VariableLocation]

  private var stackPointer = 0

  def allocateRegister(): (Register, List[AssemblyLine]) = {
    var result: Register = R4
    var instructions: List[AssemblyLine] = List.empty[AssemblyLine]

    /* If there are available registers, allocate the first one */
    if (availableRegisters.nonEmpty) {
      result = availableRegisters.head
    } else {
      /* If there are no available registers, find a variable that is stored in a register and
         store it in memory */
      val regsInUse = allRegisters diff availableRegisters
      for (reg <- regsInUse) {
        val varName: Option[String] = varMap.find { case (_, v) => v.register == reg }.map(_._1)
        varName match {
          case Some(name) => 
            stackPointer -= 4
            varMap(name) = VariableLocation(FP, stackPointer, varMap(name).size, varMap(name)._type)
            instructions = List(StoreInstr(reg, FP, ImmVal(stackPointer)))
            return (reg, instructions)
          case _ =>
        }
      }
    }

    println(s"Allocated register: $result")

    /* Remove the allocated register from the list of available registers */
    if (availableRegisters.length < 2) {
      availableRegisters = List.empty[Register]
    } else {
      availableRegisters = availableRegisters.tail
    }
    (result, instructions)
  }

  def lookupLocation(varName: String): Option[VariableLocation] = {
    varMap.get(varName)
  }

  def setLocation(varName: String, location: VariableLocation): Unit = {
    varMap(varName) = location
  }

  def removeVariable(varName: String): Unit = {
    varMap -= varName
  }

  def deallocateRegister(register: Register): Unit = {
    println(s"Deallocated register: $register")

    /* Add the deallocated register to the list of available registers */
    availableRegisters = register :: availableRegisters
  }
}