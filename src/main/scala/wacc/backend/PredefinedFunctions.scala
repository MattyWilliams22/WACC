package wacc.backend

import wacc.backend.CodeGenerator.refFunctions

/*
 * This file contains functions taken from the reference compiler provided for the WACC language. We have
 * converted these functions into our own Internal Representation and extracted a generic function wrapper
 * for all of them.
 * Source: https://teaching.doc.ic.ac.uk/wacc_compiler/
 * Author: Jamie Willis
 */
object PredefinedFunctions {

  /* Generates the assembly code for a generic function, given the specified parameters */
  private def functionWrapper(funcName: String, funcLabel: String, stringLiterals: List[Instruction],
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
      BInstr("exit", noCondition, storeReturnAddr = true)
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
      AscizInstr(s".L._print${_type}_str0", StringLiteral(formatSpecifier))
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      Mov(R1, R0),
      AdrInstr(R0, s".L._print${_type}_str0"),
      BInstr("printf", noCondition, storeReturnAddr = true),
      Mov(R0, ImmVal(0)),
      BInstr("fflush", noCondition, storeReturnAddr = true),
    )

    functionWrapper(funcName, labelName, stringLiterals, funcBody)
  }

  /* Generates the assembly code for printing a string */
  lazy val printStrFunc: List[Instruction] = {
    val funcName = "Print string"

    val labelName = "_prints"

    val stringLiterals = List(
      AscizInstr(".L._prints_str0", StringLiteral("%.*s"))
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      Mov(R2, R0),
      Ldr(R1, Addr(R0, ImmVal(-4))),
      AdrInstr(R0, ".L._prints_str0"),
      BInstr("printf", noCondition, storeReturnAddr = true),
      Mov(R0, ImmVal(0)),
      BInstr("fflush", noCondition, storeReturnAddr = true),
    )

    functionWrapper(funcName, labelName, stringLiterals, funcBody)
  }

  /* Generates the assembly code for printing a pair */
  lazy val printPairFunc: List[Instruction] = {
    val funcName = "Print pair"

    val labelName = "_printp"

    val stringLiterals = List(
      AscizInstr(".L._printp_str0", StringLiteral("%p")),
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      Mov(R1, R0),
      AdrInstr(R0, ".L._printp_str0"),
      BInstr("printf", noCondition, storeReturnAddr = true),
      Mov(R0, ImmVal(0)),
      BInstr("fflush", noCondition, storeReturnAddr = true),
    )

    functionWrapper(funcName, labelName, stringLiterals, funcBody)
  }

  /* Generates the assembly code for printing a bool */
  lazy val printBoolFunc: List[Instruction] = {
    val funcName = "Print bool"

    val labelName = "_printb"

    val stringLiterals = List(
      AscizInstr(".L._printb_str0", StringLiteral("false")),
      AscizInstr(".L._printb_str1", StringLiteral("true")),
      AscizInstr(".L._printb_str2", StringLiteral("%.*s")),
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
      Ldr(R1, Addr(R2, ImmVal(-4))),
      AdrInstr(R0, ".L._printb_str2"),
      BInstr("printf", noCondition, storeReturnAddr = true),
      Mov(R0, ImmVal(0)),
      BInstr("fflush", noCondition, storeReturnAddr = true),
    )

    functionWrapper(funcName, labelName, stringLiterals, funcBody)
  }

  /* Generates the assembly code for printing with a newline */
  lazy val printLnFunc: List[Instruction] = {
    val funcName = "Println"

    val funcLabel = "_println"

    val stringLiterals = List(
      AscizInstr(".L._println_str0", StringLiteral("")),
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._println_str0"),
      BInstr("puts", noCondition, storeReturnAddr = true),
      Mov(R0, ImmVal(0)),
      BInstr("fflush", noCondition, storeReturnAddr = true),
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
      BInstr("malloc", noCondition, storeReturnAddr = true),
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
      AscizInstr(".L._readi_str0", StringLiteral("%d")),
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      SubInstr(SP, SP, ImmVal(8)),
      StrInstr(R0, Addr(SP, ImmVal(0))),
      Mov(R1, SP),
      AdrInstr(R0, ".L._readi_str0"),
      BInstr("scanf", noCondition, storeReturnAddr = true),
      Ldr(R0, Addr(SP, ImmVal(0))),
      AddInstr(SP, SP, ImmVal(8)),
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for reading a character */
  lazy val readCharFunc: List[Instruction] = {
    val funcName = "Read char"

    val funcLabel = "_readc"

    val stringLiterals = List(
      AscizInstr(".L._readc_str0", StringLiteral(" %c")),
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      SubInstr(SP, SP, ImmVal(8)),
      StrInstr(R0, Addr(SP, ImmVal(0))),
      Mov(R1, SP),
      AdrInstr(R0, ".L._readc_str0"),
      BInstr("scanf", noCondition, storeReturnAddr = true),
      Ldr(R0, Addr(SP, ImmVal(0))),
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
      BInstr("free", noCondition, storeReturnAddr = true),
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
      BInstr("_errNull", EQcond, storeReturnAddr = true),
      BInstr("_free", noCondition, storeReturnAddr = true),
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for storing an array */
  lazy val arrayStore1Func: List[Instruction] = {
    refFunctions += errorOutOfBoundsFunc
    List(
      NewLine(),
      Comment("Array store function"),
      Label("_arrStore1"),
      Push(List(LR)),
      CmpInstr(R0, ImmVal(0)),
      Mov(R1, R0, LTcond),
      BInstr("_errOutOfBounds", LTcond, storeReturnAddr = true),
      Ldr(LR, Addr(R3, ImmVal(-4))),
      CmpInstr(R0, LR),
      Mov(R1, R0, GEcond),
      BInstr("_errOutOfBounds", GEcond, storeReturnAddr = true),
      StrInstr(R8, Addr(R3, R0), OneByte),
      Pop(List(PC))
    )
  }

  /* Generates the assembly code for loading an array */
  lazy val arrayLoad4Func: List[Instruction] = {
    refFunctions += errorOutOfBoundsFunc
    List(
      NewLine(),
      Comment("Array load function"),
      Label("_arrLoad4"),
      Push(List(LR)),
      CmpInstr(R0, ImmVal(0)),
      Mov(R1, R0, LTcond),
      BInstr("_errOutOfBounds", LTcond, storeReturnAddr = true),
      Ldr(LR, Addr(R3, ImmVal(-4))),
      CmpInstr(R0, LR),
      Mov(R1, R0, GEcond),
      BInstr("_errOutOfBounds", GEcond, storeReturnAddr = true),
      Ldr(R3, Addr(R3, RegShift(R0, ShiftLeft(2)))),
      Pop(List(PC))
    )
  }

  /* Generates the assembly code for storing an array */
  lazy val arrayStore4Func: List[Instruction] = {
    refFunctions += errorOutOfBoundsFunc

    List(
      NewLine(),
      Comment("Array store function"),
      Label("_arrStore4"),
      Push(List(LR)),
      CmpInstr(R0, ImmVal(0)),
      Mov(R1, R0, LTcond),
      BInstr("_errOutOfBounds", LTcond, storeReturnAddr = true),
      Ldr(LR, Addr(R3, ImmVal(-4))),
      CmpInstr(R0, LR),
      Mov(R1, R0, GEcond),
      BInstr("_errOutOfBounds", GEcond, storeReturnAddr = true),
      StrInstr(R8, Addr(R3, RegShift(R0, ShiftLeft(2)))),
      Pop(List(PC))
    )
  }

  /* Generates the assembly code for handling an Out of Memory error */
  private lazy val errorOutOfMemoryFunc: List[Instruction] = {
    refFunctions += printStrFunc

    val funcName = "Error out of memory"

    val funcLabel = "_errOutOfMemory"

    val stringLiterals = List(
      AscizInstr(".L._errOutOfMemory_str0", ErrorMessage(OutOfMemoryErr))
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errOutOfMemory_str0"),
      BInstr("_prints", noCondition, storeReturnAddr = true),
      Mov(R0, ImmVal(255)),
      BInstr("exit", noCondition, storeReturnAddr = true),
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for handling an Out of Bounds error */
  private lazy val errorOutOfBoundsFunc: List[Instruction] = {
    val funcName = "Error out of bounds"

    val funcLabel = "_errOutOfBounds"

    val stringLiterals = List(
      AscizInstr(".L._errOutOfBounds_str0", ErrorMessage(IndexOutOfBoundsErr))
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errOutOfBounds_str0"),
      BInstr("printf", noCondition, storeReturnAddr = true),
      Mov(R0, ImmVal(0)),
      BInstr("fflush", noCondition, storeReturnAddr = true),
      Mov(R0, ImmVal(255)),
      BInstr("exit", noCondition, storeReturnAddr = true),
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for handling a Null Pointer error */
  lazy val errorNullFunc: List[Instruction] = {
    refFunctions += printStrFunc

    val funcName = "Error null pointer"

    val funcLabel = "_errNull"

    val stringLiterals = List(
      AscizInstr(".L._errNull_str0", ErrorMessage(NullReferenceErr))
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errNull_str0"),
      BInstr("_prints", noCondition, storeReturnAddr = true),
      Mov(R0, ImmVal(255)),
      BInstr("exit", noCondition, storeReturnAddr = true),
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for handling an Overflow error */
  lazy val errorOverflowFunc: List[Instruction] = {
    refFunctions += printStrFunc

    val funcName = "Error overflow"

    val funcLabel = "_errOverflow"

    val stringLiterals = List(
      AscizInstr(".L._errOverflow_str0", ErrorMessage(IntegerOverflowUnderflowErr))
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errOverflow_str0"),
      BInstr("_prints", noCondition, storeReturnAddr = true),
      Mov(R0, ImmVal(255)),
      BInstr("exit", noCondition, storeReturnAddr = true),
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }

  /* Generates the assembly code for handling a Division by Zero error */
  lazy val errorDivByZeroFunc: List[Instruction] = {
    refFunctions += printStrFunc

    val funcName = "Error division by zero"

    val funcLabel = "_errDivZero"

    val stringLiterals = List(
      AscizInstr(".L._errDivZero_str0", ErrorMessage(DivByZeroErr))
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errDivZero_str0"),
      BInstr("_prints", noCondition, storeReturnAddr = true),
      Mov(R0, ImmVal(255)),
      BInstr("exit", noCondition, storeReturnAddr = true),
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
      AscizInstr(".L._errBadChar_str0", ErrorMessage(CharNotInRangeErr))
    )

    val funcBody = List(
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errBadChar_str0"),
      BInstr("printf", noCondition, storeReturnAddr = true),
      Mov(R0, ImmVal(0)),
      BInstr("fflush", noCondition, storeReturnAddr = true),
      Mov(R0, ImmVal(255)),
      BInstr("exit", noCondition, storeReturnAddr = true),
    )

    functionWrapper(funcName, funcLabel, stringLiterals, funcBody)
  }
}
