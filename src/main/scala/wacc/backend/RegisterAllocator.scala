package wacc.backend

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import wacc.ASTNodes._

/* Represents an object that is used to allocate general purpose registers within the ARM architecture */
sealed trait RegisterAllocator {
  def allocateRegister(): (Register, ListBuffer[Instruction])

  def deallocateRegister(register: Register): Unit

  def setLocation(varName: String, location: VariableLocation): Unit

  def lookupLocation(varName: String): Option[VariableLocation]

  def getNewRegisterAllocator(): RegisterAllocator
}

/* Represents the location of a variable in the program state, as well as the variable's type */
case class VariableLocation(register: Register, offset: Int, size: Int, _type: Type)

/* Basic register allocator that allocates registers by always putting the result in the register
   that is at the front of the list of available registers */
class BasicRegisterAllocator extends RegisterAllocator {
  /* List of all registers that can be allocated by register allocator */
  private val allRegisters: List[Register] = List(R4, R5, R6, R7, R8, R9, R10, R1, R2, R3, IP)

  /* List of current available registers */
  private var availableRegisters: List[Register] = allRegisters

  /* Map of variable names to their locations where they are stored */
  private val varMap: mutable.Map[String, VariableLocation] = mutable.Map.empty[String, VariableLocation]

  /* Stores the current stack pointer */
  private var stackPointer = 0

  /* Allocate a register */
  def allocateRegister(): (Register, ListBuffer[Instruction]) = {
    var result: Register = R4
    val instructions = ListBuffer[Instruction]()

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
            instructions += StrInstr(reg, Addr(FP, ImmVal(stackPointer)))
            return (reg, instructions)
          case _ =>
        }
      }
    }

    /* Remove the allocated register from the list of available registers */
    if (availableRegisters.length < 2) {
      availableRegisters = List.empty[Register]
    } else {
      availableRegisters = availableRegisters.tail
    }
    (result, instructions)
  }

  /* Lookup the location of a variable in the varMap */
  def lookupLocation(varName: String): Option[VariableLocation] = {
    varMap.get(varName)
  }

  /* Set the location of a variable in the varMap */
  def setLocation(varName: String, location: VariableLocation): Unit = {
    varMap(varName) = location
  }

  /* Deallocate a register */
  def deallocateRegister(register: Register): Unit = {
    /* Add the deallocated register to the list of available registers */
    availableRegisters = register :: availableRegisters
  }

  def getNewRegisterAllocator(): RegisterAllocator = {
    new BasicRegisterAllocator()
  }
}

class TemporaryRegisterAllocator extends RegisterAllocator {
  private var registerCount: Int = 0

  /* Map of variable names to their locations where they are stored */
  private val varMap: mutable.Map[String, VariableLocation] = mutable.Map.empty[String, VariableLocation]

  /* Stores the current stack pointer */
  private var stackPointer = 0

  /* Allocate a register */
  def allocateRegister(): (Register, ListBuffer[Instruction]) = {
    val result = T(registerCount)
    registerCount += 1
    (result, ListBuffer[Instruction]())
  }

  /* Lookup the location of a variable in the varMap */
  def lookupLocation(varName: String): Option[VariableLocation] = {
    varMap.get(varName)
  }

  /* Set the location of a variable in the varMap */
  def setLocation(varName: String, location: VariableLocation): Unit = {
    varMap(varName) = location
  }

  /* Deallocate a register */
  def deallocateRegister(register: Register): Unit = {
    
  }

  def getNewRegisterAllocator(): RegisterAllocator = {
    new TemporaryRegisterAllocator()
  }
}
