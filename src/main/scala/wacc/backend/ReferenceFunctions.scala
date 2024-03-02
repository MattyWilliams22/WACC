package wacc.backend

import wacc.backend.CodeGenerator.refFunctions

object ReferenceFunctions {

  /* Generates the assembly code for a generic function, given the specified parameters */
  def functionWrapper(funcName: String, funcLabel: String, stringLiterals: List[Instruction],
                      funcBody: List[Instruction]): List[Instruction] = {
    NewLine() ::
    stringLiterals ++
    List(
      Command("align 4", 0),
      Comment(s"$funcName function"),
      Label(funcLabel),
      Push(List(FP, LR)),
      Mov(FP, SP)) ++
    funcBody ++
    List(
      Mov(SP, FP),
      Pop(List(FP, PC)))
  }


  /* Generates the assembly code for exiting a program with a given exit code */
  lazy val exitFunc: List[Instruction] = {
    val funcName = "Exit"

    val funcLabel = "_exit"

    val stringLiterals = List()

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      BlInstr("exit")
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for printing a character or an integer */
  def printCharOrIntFunc(isChar: Boolean): List[Instruction] = {
    var _type: String = ""
    var formatSpecifier: String = ""

    /* Set the type and format specifier based on whether the value to be printed
       is a character or an integer */
    if (isChar) {
      _type = "c"
      formatSpecifier = "%c"
    } else {
      _type = "i"
      formatSpecifier = "%d"
    }

    val funcName = s"Print${_type}"

    val labelName = s"_print${_type}"

    val stringLiterals = List(
      AscizInstr(s".L._print${_type}_str0", formatSpecifier)
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      Mov(R1, R0),
      AdrInstr(R0, s".L._print${_type}_str0"),
      BlInstr("printf"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
    )

    functionWrapper(funcName, labelName, stringLiterals, funcBody)
  }

  /* Generates the assembly code for printing a string */
  lazy val printStrFunc: List[Instruction] = {
    val funcName = "Print string"

    val labelName = "_prints"

    val stringLiterals = List(
      AscizInstr(".L._prints_str0", "%.*s")
    )

    val funcBody = List(
      Push(List(FP, LR)),
      Mov(FP, SP),
      BicInstr(SP, SP, ImmVal(7)),
      Mov(R2, R0),
      LdrAddr(R1, R0, ImmVal(-4)),
      AdrInstr(R0, ".L._prints_str0"),
      BlInstr("printf"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
    )

    functionWrapper(funcName, labelName, stringLiterals, funcBody)
  }

  /* Generates the assembly code for printing a pair */
  lazy val printPairFunc: List[Instruction] = {
    val funcName = "Print pair"

    val labelName = "_printp"

    val stringLiterals = List(
      AscizInstr(".L._printp_str0", "%p"),
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      Mov(R1, R0),
      AdrInstr(R0, ".L._printp_str0"),
      BlInstr("printf"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
    )

    functionWrapper(funcName, labelName, stringLiterals, funcBody)
  }

  /* Generates the assembly code for printing a bool */
  lazy val printBoolFunc: List[Instruction] = {
    val funcName = "Print bool"

    val labelName = "_printb"

    val stringLiterals = List(
      AscizInstr(".L._printb_str0", "false"),
      AscizInstr(".L._printb_str1", "true"),
      AscizInstr(".L._printb_str2", "%.*s"),
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      CmpInstr(R0, ImmVal(0)),
      BInstr(".L._printb0", NEcond),
      AdrInstr(R2, ".L._printb_str0"),
      BInstr(".L._printb1"),
      Label(".L._printb0"),
      AdrInstr(R2, ".L._printb_str1"),
      Label(".L._printb1"),
      LdrAddr(R1, R2, ImmVal(-4)),
      AdrInstr(R0, ".L._printb_str2"),
      BlInstr("printf"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
    )

    functionWrapper(funcName, labelName, stringLiterals, funcBody)
  }

  /* Generates the assembly code for printing with a newline */
  lazy val printLnFunc: List[Instruction] = {
    val funcName = "Println"

    val funcLabel = "_println"

    val stringLiterals = List(
      AscizInstr(".L._println_str0", ""),
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._println_str0"),
      BlInstr("puts"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for malloc */
  lazy val mallocFunc: List[Instruction] = {
    refFunctions += errorOutOfMemoryFunc

    val funcName = "Malloc"

    val funcLabel = "_malloc"

    val stringLiterals = List()

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      BlInstr("malloc"),
      CmpInstr(R0, ImmVal(0)),
      BInstr("_errOutOfMemory", EQcond),
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for reading an integer */
  lazy val readIntFunc: List[Instruction] = {
    val funcName = "Read int"

    val funcLabel = "_readi"

    val stringLiterals = List(
      AscizInstr(".L._readi_str0", "%d"),
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      SubInstr(SP, SP, ImmVal(8)),
      StoreInstr(R0, SP, ImmVal(0)),
      Mov(R1, SP),
      AdrInstr(R0, ".L._readi_str0"),
      BlInstr("scanf"),
      LdrAddr(R0, SP, ImmVal(0)),
      AddInstr(SP, SP, ImmVal(8)),
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for reading a character */
  lazy val readCharFunc: List[Instruction] = {
    val funcName = "Read char"

    val funcLabel = "_readc"

    val stringLiterals = List(
      AscizInstr(".L._readc_str0", " %c"),
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      SubInstr(SP, SP, ImmVal(8)),
      StoreInstr(R0, SP, ImmVal(0)),
      Mov(R1, SP),
      AdrInstr(R0, ".L._readc_str0"),
      BlInstr("scanf"),
      LdrAddr(R0, SP, ImmVal(0)),
      AddInstr(SP, SP, ImmVal(8)),
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for freeing memory */
  lazy val freeFunc: List[Instruction] = {
    val funcName = "Free"

    val funcLabel = "_free"

    val stringLiterals = List()

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      BlInstr("free"),
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for freeing a pair */
  lazy val freePairFunc: List[Instruction] = {
    refFunctions += freeFunc
    refFunctions += errorNullFunc

    val funcName = "Free pair"

    val funcLabel = "_freepair"

    val stringLiterals = List()

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      CmpInstr(R0, ImmVal(0)),
      BlInstr("_errNull", EQcond),
      BlInstr("_free", noCondition),
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for loading an array */
  lazy val arrayLoad4Func: List[Instruction] = {
    refFunctions += errorOutOfBoundsFunc

    val funcName = "Array load"

    val funcLabel = "_arrLoad4"

    val stringLiterals = List()

    val funcBody = List(
      Push(List(LR)),
      CmpInstr(IP, ImmVal(0)),
      Mov(R1, IP, LTcond),
      BlInstr("_errOutOfBounds", LTcond),
      LdrAddr(LR, R3, ImmVal(-4)),
      CmpInstr(IP, LR),
      Mov(R1, IP, GEcond),
      BlInstr("_errOutOfBounds", GEcond),
      LdrShift(R3, R3, IP, ShiftLeft(2)),
      Pop(List(PC))
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for storing an array */
  lazy val arrayStore4Func: List[Instruction] = {
    refFunctions += errorOutOfBoundsFunc

    val funcName = "Array store"

    val funcLabel = "_arrStore4"

    val stringLiterals = List()

    val funcBody = List(
      Push(List(LR)),
      CmpInstr(IP, ImmVal(0)),
      Mov(R1, IP, LTcond),
      BlInstr("_errOutOfBounds", LTcond),
      LdrAddr(LR, R3, ImmVal(-4)),
      CmpInstr(IP, LR),
      Mov(R1, IP, GEcond),
      BlInstr("_errOutOfBounds", GEcond),
      StoreShift(R8, R3, IP, ShiftLeft(2)),
      Pop(List(PC))
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for handling an Out of Memory error */
  private lazy val errorOutOfMemoryFunc: List[Instruction] = {
    refFunctions += printStrFunc

    val funcName = "Error out of memory"

    val funcLabel = "_errOutOfMemory"

    val stringLiterals = List(
      AscizInstr(".L._errOutOfMemory_str0", "Error: Out of memory")
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errOutOfMemory_str0"),
      BlInstr("_prints"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit"),
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for handling an Out of Bounds error */
  private lazy val errorOutOfBoundsFunc: List[Instruction] = {
    val funcName = "Error out of bounds"

    val funcLabel = "_errOutOfBounds"

    val stringLiterals = List(
      AscizInstr(".L._errOutOfBounds_str0", "Error: Array index out of bounds")
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errOutOfBounds_str0"),
      BlInstr("printf"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit")
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for handling a Null Pointer error */
  lazy val errorNullFunc: List[Instruction] = {
    refFunctions += printStrFunc

    val funcName = "Error null pointer"

    val funcLabel = "_errNull"

    val stringLiterals = List(
      AscizInstr(".L._errNull_str0", "Error: Null pair de-referenced")
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errNull_str0"),
      BlInstr("_prints"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit")
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for handling an Overflow error */
  lazy val errorOverflowFunc: List[Instruction] = {
    refFunctions += printStrFunc

    val funcName = "Error overflow"

    val funcLabel = "_errOverflow"

    val stringLiterals = List(
      AscizInstr(".L._errOverflow_str0", "Error: Integer overflow or underflow occurred")
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errOverflow_str0"),
      BlInstr("_prints"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit")
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for handling a Division by Zero error */
  lazy val errorDivByZeroFunc: List[Instruction] = {
    refFunctions += printStrFunc

    val funcName = "Error division by zero"

    val funcLabel = "_errDivZero"

    val stringLiterals = List(
      AscizInstr(".L._errDivZero_str0", "Error: Division by zero")
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errDivZero_str0"),
      BlInstr("_prints"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit")
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for handling a Bad Character error, so when a character
     is not within the acceptable ASCII range */
  lazy val errorBadCharFunc: List[Instruction] = {
    refFunctions += printStrFunc

    val funcName = "Error bad character"

    val funcLabel = "_errBadChar"

    val stringLiterals = List(
      AscizInstr(".L._errBadChar_str0", "fatal error: int %d is not ascii character 0-127 \n")
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errBadChar_str0"),
      BlInstr("printf"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit")
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for storing an array */
  lazy val arrayStore1Func: List[Instruction] = {
    refFunctions += errorOutOfBoundsFunc

    val funcName = "Array store"

    val funcLabel = "_arrStore1"

    val stringLiterals = List()

    val funcBody = List(
      Push(List(LR)),
      CmpInstr(IP, ImmVal(0)),
      Mov(R1, IP, LTcond),
      BlInstr("_errOutOfBounds", LTcond),
      LdrAddr(LR, R3, ImmVal(-4)),
      CmpInstr(IP, LR),
      Mov(R1, IP, GEcond),
      BlInstr("_errOutOfBounds", GEcond),
      StoreInstr(R8, R3, IP, OneByte),
      Pop(List(PC))
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }
}
