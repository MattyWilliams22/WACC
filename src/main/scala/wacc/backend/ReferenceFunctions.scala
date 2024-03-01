package wacc.backend

import wacc.backend.CodeGenerator.refFunctions

object ReferenceFunctions {
  /* Generates the assembly code for exiting a program with a given exit code */
  lazy val exitFunc: List[Instruction] = List(
    NewLine(),
    Comment("Exit function"),
    Label("_exit"),
    Push(List(FP, LR)),
    Mov(FP, SP),
    BicInstr(SP, SP, ImmVal(7)),
    BlInstr("exit"),
    Mov(SP, FP),
    Pop(List(FP, PC))
  )

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

    List(
      NewLine(),
      AscizInstr(s".L._print${_type}_str0", formatSpecifier),
      Command("align 4", 0),
      Comment(s"Print${_type} function"),
      Label(s"_print${_type}"),
      Push(List(FP, LR)),
      Mov(FP, SP),
      BicInstr(SP, SP, ImmVal(7)),
      Mov(R1, R0),
      AdrInstr(R0, s".L._print${_type}_str0"),
      BlInstr("printf"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
      Mov(SP, FP),
      Pop(List(FP, PC))
    )
  }

  /* Generates the assembly code for printing a string */
  lazy val printStrFunc: List[Instruction] = {
    List(
      NewLine(),
      AscizInstr(".L._prints_str0", "%.*s"),
      Command("align 4", 0),
      Comment("Print string function"),
      Label("_prints"),
      Push(List(FP, LR)),
      Mov(FP, SP),
      BicInstr(SP, SP, ImmVal(7)),
      Mov(R2, R0),
      LdrAddr(R1, R0, ImmVal(-4)),
      AdrInstr(R0, ".L._prints_str0"),
      BlInstr("printf"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
      Mov(SP, FP),
      Pop(List(FP, PC))
    )
  }

  /* Generates the assembly code for printing a pair */
  lazy val printPairFunc: List[Instruction] = {
    List(
      NewLine(),
      AscizInstr(".L._printp_str0", "%p"),
      Command("align 4", 0),
      Comment("Print pair function"),
      Label("_printp"),
      Push(List(FP, LR)),
      Mov(FP, SP),
      BicInstr(SP, SP, ImmVal(7)),
      Mov(R1, R0),
      AdrInstr(R0, ".L._printp_str0"),
      BlInstr("printf"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
      Mov(SP, FP),
      Pop(List(FP, PC))
    )
  }

  /* Generates the assembly code for printing a bool */
  lazy val printBoolFunc: List[Instruction] = {
    List(
      NewLine(),
      AscizInstr(".L._printb_str0", "false"),
      AscizInstr(".L._printb_str1", "true"),
      AscizInstr(".L._printb_str2", "%.*s"),
      Command("align 4", 0),
      Comment("Print bool function"),
      Label("_printb"),
      Push(List(FP, LR)),
      Mov(FP, SP),
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
      Mov(SP, FP),
      Pop(List(FP, PC))
    )
  }

  /* Generates the assembly code for printing with a newline */
  lazy val printLnFunc: List[Instruction] = {
    List(
      NewLine(),
      AscizInstr(".L._println_str0", ""),
      Command("align 4", 0),
      Comment("Println function"),
      Label("_println"),
      Push(List(FP, LR)),
      Mov(FP, SP),
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._println_str0"),
      BlInstr("puts"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
      Mov(SP, FP),
      Pop(List(FP, PC))
    )
  }

  /* Generates the assembly code for malloc */
  lazy val mallocFunc: List[Instruction] = {
    refFunctions += errorOutOfMemoryFunc
    List(
      NewLine(),
      Comment("Malloc function"),
      Label("_malloc"),
      Push(List(FP, LR)),
      Mov(FP, SP),
      BicInstr(SP, SP, ImmVal(7)),
      BlInstr("malloc"),
      CmpInstr(R0, ImmVal(0)),
      BInstr("_errOutOfMemory", EQcond),
      Mov(SP, FP),
      Pop(List(FP, PC))
    )
  }

  /* Generates the assembly code for reading an integer */
  lazy val readIntFunc: List[Instruction] = {
    List(
      NewLine(),
      AscizInstr(".L._readi_str0", "%d"),
      Command("align 4", 0),
      Comment("Read int function"),
      Label("_readi"),
      Push(List(FP, LR)),
      Mov(FP, SP),
      BicInstr(SP, SP, ImmVal(7)),
      SubInstr(SP, SP, ImmVal(8)),
      StoreInstr(R0, SP, ImmVal(0)),
      Mov(R1, SP),
      AdrInstr(R0, ".L._readi_str0"),
      BlInstr("scanf"),
      LdrAddr(R0, SP, ImmVal(0)),
      AddInstr(SP, SP, ImmVal(8)),
      Mov(SP, FP),
      Pop(List(FP, PC))
    )
  }

  /* Generates the assembly code for reading a character */
  lazy val readCharFunc: List[Instruction] = {
    List(
      NewLine(),
      AscizInstr(".L._readc_str0", " %c"),
      Command("align 4", 0),
      Comment("Read char function"),
      Label("_readc"),
      Push(List(FP, LR)),
      Mov(FP, SP),
      BicInstr(SP, SP, ImmVal(7)),
      SubInstr(SP, SP, ImmVal(8)),
      StoreInstr(R0, SP, ImmVal(0)),
      Mov(R1, SP),
      AdrInstr(R0, ".L._readc_str0"),
      BlInstr("scanf"),
      LdrAddr(R0, SP, ImmVal(0)),
      AddInstr(SP, SP, ImmVal(8)),
      Mov(SP, FP),
      Pop(List(FP, PC))
    )
  }

  /* Generates the assembly code for freeing memory */
  lazy val freeFunc: List[Instruction] = List(
    NewLine(),
    Comment("Free function"),
    Label("_free"),
    Push(List(FP, LR)),
    Mov(FP, SP),
    BicInstr(SP, SP, ImmVal(7)),
    BlInstr("free"),
    Mov(SP, FP),
    Pop(List(FP, PC))
  )

  /* Generates the assembly code for freeing a pair */
  lazy val freePairFunc: List[Instruction] = {
    refFunctions += freeFunc
    refFunctions += errorNullFunc
    List(
      NewLine(),
      Comment("Free pair function"),
      Label("_freepair"),
      Push(List(FP, LR)),
      Mov(FP, SP),
      BicInstr(SP, SP, ImmVal(7)),
      CmpInstr(R0, ImmVal(0)),
      BlInstr("_errNull", EQcond),
      BlInstr("_free", noCondition),
      Mov(SP, FP),
      Pop(List(FP, PC))
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
      CmpInstr(R10, ImmVal(0)),
      Mov(R1, R10, LTcond),
      BlInstr("_errOutOfBounds", LTcond),
      LdrAddr(LR, R3, ImmVal(-4)),
      CmpInstr(R10, LR),
      Mov(R1, R10, GEcond),
      BlInstr("_errOutOfBounds", GEcond),
      LdrShift(R3, R3, R10, ShiftLeft(2)),
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
      CmpInstr(R10, ImmVal(0)),
      Mov(R1, R10, LTcond),
      BlInstr("_errOutOfBounds", LTcond),
      LdrAddr(LR, R3, ImmVal(-4)),
      CmpInstr(R10, LR),
      Mov(R1, R10, GEcond),
      BlInstr("_errOutOfBounds", GEcond),
      StoreShift(R8, R3, R10, ShiftLeft(2)),
      Pop(List(PC))
    )
  }

  /* Generates the assembly code for handling an Out of Memory error */
  private lazy val errorOutOfMemoryFunc: List[Instruction] = {
    refFunctions += printStrFunc
    List(
      NewLine(),
      Comment("Error out of memory function"),
      AscizInstr(".L._errOutOfMemory_str0", "Error: Out of memory"),
      Command("align 4", 0),
      Label("_errOutOfMemory"),
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errOutOfMemory_str0"),
      BlInstr("_prints"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit"),
    )
  }

  /* Generates the assembly code for handling an Out of Bounds error */
  private lazy val errorOutOfBoundsFunc: List[Instruction] = List(
    NewLine(),
    Comment("Error out of bounds function"),
    AscizInstr(".L._errOutOfBounds_str0", "Error: Array index out of bounds"),
    Command("align 4", 0),
    Label("_errOutOfBounds"),
    BicInstr(SP, SP, ImmVal(7)),
    AdrInstr(R0, ".L._errOutOfBounds_str0"),
    BlInstr("printf"),
    Mov(R0, ImmVal(0)),
    BlInstr("fflush"),
    Mov(R0, ImmVal(255)),
    BlInstr("exit")
  )

  /* Generates the assembly code for handling a Null Pointer error */
  lazy val errorNullFunc: List[Instruction] = {
    refFunctions += printStrFunc
    List(
      NewLine(),
      Comment("Error null pointer function"),
      AscizInstr(".L._errNull_str0", "Error: Null pair dereferenced"),
      Command("align 4", 0),
      Label("_errNull"),
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errNull_str0"),
      BlInstr("_prints"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit")
    )
  }

  /* Generates the assembly code for handling an Overflow error */
  lazy val errorOverflowFunc: List[Instruction] = {
    refFunctions += printStrFunc
    List(
      NewLine(),
      Comment("Error overflow function"),
      AscizInstr(".L._errOverflow_str0", "Error: Integer overflow or underflow occured"),
      Command("align 4", 0),
      Label("_errOverflow"),
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errOverflow_str0"),
      BlInstr("_prints"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit")
    )
  }

  /* Generates the assembly code for handling a Division by Zero error */
  lazy val errorDivByZeroFunc: List[Instruction] = {
    refFunctions += printStrFunc
    List(
      NewLine(),
      Comment("Error division by zero function"),
      AscizInstr(".L._errDivZero_str0", "Error: Division by zero"),
      Command("align 4", 0),
      Label("_errDivZero"),
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errDivZero_str0"),
      BlInstr("_prints"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit")
    )
  }

  /* Generates the assembly code for handling a Bad Character error, so when a character
     is not within the acceptable ASCII range */
  lazy val errorBadCharFunc: List[Instruction] = {
    refFunctions += printStrFunc
    List(
      NewLine(),
      Comment("Error bad character function"),
      AscizInstr(".L._errBadChar_str0", "fatal error: int %d is not ascii character 0-127 \n"),
      Command("align 4", 0),
      Label("_errBadChar"),
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errBadChar_str0"),
      BlInstr("printf"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit")
    )
  }

  /* Generates the assembly code for storing an array */
  lazy val arrayStore1Func: List[Instruction] = {
    refFunctions += errorOutOfBoundsFunc
    List(
      NewLine(),
      Comment("Array store function"),
      Label("_arrStore1"),
      Push(List(LR)),
      CmpInstr(R10, ImmVal(0)),
      Mov(R1, R10, LTcond),
      BlInstr("_errOutOfBounds", LTcond),
      LdrAddr(LR, R3, ImmVal(-4)),
      CmpInstr(R10, LR),
      Mov(R1, R10, GEcond),
      BlInstr("_errOutOfBounds", GEcond),
      StoreInstr(R8, R3, R10, OneByte),
      Pop(List(PC))
    )
  }
}
