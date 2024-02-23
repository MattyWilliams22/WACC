package wacc.backend

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import wacc.ASTNodes._
import wacc.backend.Instructions._

object CodeGenerator {
  private var labelCounter: Int = -1
  private var stringCounter: Int = -1
  private val refFunctions: mutable.Set[List[AssemblyLine]] = mutable.Set()
  private val stringPool: mutable.Set[AscizInstr] = mutable.Set()

  private def getStringLabel: String = {
    stringCounter += 1
    ".L._str" + stringCounter
  }

  private lazy val exitFunc: List[AssemblyLine] = List(
    NewLine(),
    Comment("Exit function"),
    Label("_exit"),
    PushMultiple(List(FP, LR)),
    Mov(FP, SP),
    BicInstr(SP, SP, ImmVal(7)),
    BlInstr("exit"),
    Mov(SP, FP),
    PopMultiple(List(FP, PC))
  )

  private def printCharOrIntFunc(isChar: Boolean): List[AssemblyLine] = {
    var _type: String = ""
    var formatSpecifier: String = ""

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
      Command("align 4"),
      Comment(s"Print${_type} function"),
      Label(s"_print${_type}"),
      PushMultiple(List(FP, LR)),
      Mov(FP, SP),
      BicInstr(SP, SP, ImmVal(7)),
      Mov(R1, R0),
      AdrInstr(R0, s".L._print${_type}_str0"),
      BlInstr("printf"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
      Mov(SP, FP),
      PopMultiple(List(FP, PC))
    )
  }

  private lazy val printStrFunc: List[AssemblyLine] = {
    List(
      NewLine(),
      AscizInstr(".L._prints_str0", "%.*s"),
      Command("align 4"),
      Comment("Print string function"),
      Label("_prints"),
      PushMultiple(List(FP, LR)),
      Mov(FP, SP),
      BicInstr(SP, SP, ImmVal(7)),
      Mov(R2, R0),
      LdrAddr(R1, R0, ImmVal(-4)),
      AdrInstr(R0, ".L._prints_str0"),
      BlInstr("printf"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
      Mov(SP, FP),
      PopMultiple(List(FP, PC))
    )
  }

  private lazy val printBoolFunc: List[AssemblyLine] = {
    List(
      NewLine(),
      AscizInstr(".L._printb_str0", "false"),
      AscizInstr(".L._printb_str1", "true"),
      AscizInstr(".L._printb_str2", "%.*s"),
      Command("align 4"),
      Comment("Print bool function"),
      Label("_printb"),
      PushMultiple(List(FP, LR)),
      Mov(FP, SP),
      BicInstr(SP, SP, ImmVal(7)),
      CmpInstr(R0, ImmVal(0)),
      BneInstr(".L._printb0"),
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
      PopMultiple(List(FP, PC))
    )
  }

  private lazy val printLnFunc: List[AssemblyLine] = {
    List(
      NewLine(),
      AscizInstr(".L._println_str0", ""),
      Command("align 4"),
      Comment("Println function"),
      Label("_println"),
      PushMultiple(List(FP, LR)),
      Mov(FP, SP),
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._println_str0"),
      BlInstr("puts"),
      Mov(R0, ImmVal(0)),
      BlInstr("fflush"),
      Mov(SP, FP),
      PopMultiple(List(FP, PC))
    )
  }

  private lazy val mallocFunc: List[AssemblyLine] = List(
    NewLine(),
    Comment("Malloc function"),
    Label("_malloc"),
    PushMultiple(List(FP, LR)),
    Mov(FP, SP),
    BicInstr(SP, SP, ImmVal(7)),
    BlInstr("malloc"),
    CmpInstr(R0, ImmVal(0)),
    BeqInstr("_errOutOfMemory"),
    Mov(SP, FP),
    PopMultiple(List(FP, PC))
  )

  private lazy val readIntFunc: List[AssemblyLine] = {
    List(
      NewLine(),
      AscizInstr(".L._readi_str0", "%d"),
      Command("align 4"),
      Comment("Read int function"),
      Label("_readi"),
      PushMultiple(List(FP, LR)),
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
      PopMultiple(List(FP, PC))
    )
  }

  private lazy val readCharFunc: List[AssemblyLine] = {
    List(
      NewLine(),
      AscizInstr(".L._readc_str0", " %c"),
      Command("align 4"),
      Comment("Read char function"),
      Label("_readc"),
      PushMultiple(List(FP, LR)),
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
      PopMultiple(List(FP, PC))
    )
  }

  private lazy val freeFunc: List[AssemblyLine] = List(
    NewLine(),
    Comment("Free function"),
    Label("_free"),
    PushMultiple(List(FP, LR)),
    Mov(FP, SP),
    BicInstr(SP, SP, ImmVal(7)),
    BlInstr("free"),
    Mov(SP, FP),
    PopMultiple(List(FP, PC))
  )

  private lazy val arrayLoad4Func: List[AssemblyLine] = List(
    NewLine(),
    Comment("Array load function"),
    Label("_arrLoad4"),
    Push(LR),
    CmpInstr(R10, ImmVal(0)),
    Movlt(R1, R10),
    BlltInstr("_errOutOfBounds"),
    LdrAddr(LR, R3, ImmVal(-4)),
    CmpInstr(R10, LR),
    Movge(R1, R10),
    BlgeInstr("_errOutOfBounds"),
    LdrShift(R3, R3, R10, ShiftLeft(2)),
    Pop(PC)
  )

  private def getUniqueLabel: String = {
    labelCounter += 1
    "L" + labelCounter
  }

  def generateAssembly(ast: ASTNode, allocator: BasicRegisterAllocator, dest: Register): List[AssemblyLine] = {

    def programGenerate(funcs: List[Function], stmts: Statement): List[AssemblyLine] = {
      val funcLines = funcs.flatMap(generateAssembly(_, allocator, dest))
      val stmtLines = generateAssembly(stmts, allocator, dest)
      List(
        Comment("Start of program"),
        Command("data")
      ) ++
      stringPool.toList ++
      List(
        Command("align 4"),
        Command("text"),
        Command("global main"),
        Label("main"), 
        PushMultiple(List(FP, LR)), 
        Mov(FP, SP)
      ) ++
      stmtLines ++
      List(
        Mov(R0, ImmVal(0)),
        PopMultiple(List(FP, PC))
      ) ++
      funcLines ++
      refFunctions.foldLeft(List[AssemblyLine]())(_ ++ _)
    }

    def functionGenerate(funcName: Ident, params: List[Param], body: Statement): List[AssemblyLine] = {
      Comment("Start of function") ::
      Label(funcName.nickname.get) ::
      PushMultiple(List(FP, LR)) ::
      Mov(FP, SP) ::
      paramsGenerate(params) ++
      generateAssembly(body, allocator, dest)
    }

    def paramsGenerate(params: List[Param]): List[AssemblyLine] = {
      var paramLines = new ListBuffer[AssemblyLine]()
      var regNum = 0
      for (param <- params) {
        val next = allocator.allocateRegister(None)
        regNum match {
          case 0 => {
            paramLines += Mov(next, R0)
            regNum += 1
          }
          case 1 => {
            paramLines += Mov(next, R1)
            regNum += 1
          }
          case 2 => {
            paramLines += Mov(next, R2)
            regNum += 1
          }
          case 3 => {
            paramLines += Mov(next, R3)
            regNum += 1
          }
          case _ => paramLines += Push(next)
        }
      }
      paramLines.toList
    }

    def declareGenerate(id: Ident, value: RValue): List[AssemblyLine] = {
      val idLines = generateAssembly(id, allocator, dest)
      val valueLines = generateAssembly(value, allocator, dest)
      Comment("Start of declare") ::
      idLines ++
      valueLines
    }

    def assignGenerate(lvalue: LValue, rvalue: RValue): List[AssemblyLine] = {
      val lvalueLines = generateAssembly(lvalue, allocator, dest)
      val next = allocator.allocateRegister(None)
      val rvalueLines = generateAssembly(rvalue, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of assign") ::
      lvalueLines ++
      rvalueLines ++
      List(
        Comment("Assign Logic"),
        Mov(dest, next)
      )
    }

    def readGenerate(lvalue: LValue): List[AssemblyLine] = {
      val _type = (lvalue.getType match {
        case BaseT("int") => {
          refFunctions += readIntFunc
          "i"
        }
        case BaseT("char") => {
          refFunctions += readCharFunc
          "c"
        }
        case _ => ""
      })
      val lvalueLines = generateAssembly(lvalue, allocator, dest)
      Comment("Start of read") ::
      lvalueLines ++
      List(
        Comment("Read Logic"),
        Mov(R0, dest),
        BlInstr(s"_read${_type}")
      )
    }

    def ifGenerate(cond: Expr, thenS: Statement, elseS: Statement): List[AssemblyLine] = {
      val elseLabel = getUniqueLabel
      val endLabel = getUniqueLabel
      val condLines = generateAssembly(cond, allocator, dest)
      val thenLines = generateAssembly(thenS, allocator, dest)
      val elseLines = generateAssembly(elseS, allocator, dest)
      Comment("Start of if statement") ::
      condLines ++
      List(
        Comment("If statement condition logic"),
        CmpInstr(dest, ImmVal(1)),
        BneInstr(elseLabel)
      ) ++
      thenLines ++
      List(BInstr(endLabel), Label(elseLabel)) ++
      elseLines ++
      List(Label(endLabel), Comment("End of if statement"))
    }

    def whileGenerate(cond: Expr, stmt: Statement): List[AssemblyLine] = {
      val startLabel = getUniqueLabel
      val endLabel = getUniqueLabel
      val condLines = generateAssembly(cond, allocator, dest)
      val stmtLines = generateAssembly(stmt, allocator, dest)
      List(Comment("Start of while loop"))
      Label(startLabel) ::
      condLines ++
      List(
        Comment("While loop condition logic"),
        // CmpInstr(dest, ImmVal(1)),
        // BneInstr(endLabel)
        BInstr(endLabel)  // Temporarily skip to end of while loop to avoid infinite loop
      ) ++ 
      stmtLines ++
      List(
        BInstr(startLabel),
        Label(endLabel),
        Comment("End of while loop")
      )
    }

    def scopeGenerate(body: Statement): List[AssemblyLine] = {
      val bodyLines = generateAssembly(body, allocator, dest)
      Comment("Start of new scope") ::
      bodyLines ++
      List(Comment("End of new scope"))
    }

    def freeGenerate(exp: Expr): List[AssemblyLine] = {
      refFunctions += freeFunc
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of free") ::
      expLines ++
      List(
        Comment("Free Logic"),
        Mov(R0, dest),
        BlInstr("_free")
      )
    }

    def returnGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of return") ::
      expLines ++
      List(
        Comment("Return Logic"),
        Mov(R0, dest),
        Mov(SP, FP),
        PopMultiple(List(FP, PC))
      )
    }

    def exitGenerate(exp: Expr): List[AssemblyLine] = {
      refFunctions += exitFunc
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of exit") ::
      expLines ++
      List(
        Comment("Exit Logic"),
        Mov(R0, dest),
        BlInstr("_exit")
      )
    }

    def printGenerate(exp: Expr): List[AssemblyLine] = {
      val _type = exp.getType match {
        case BaseT("int") =>
          refFunctions += printCharOrIntFunc(false)
          "i"
        case BaseT("char") =>
          refFunctions += printCharOrIntFunc(true)
          "c"
        case BaseT("string") =>
          refFunctions += printStrFunc
          "s"
        case ArrayT(BaseT("char"), 1) =>
          refFunctions += printStrFunc
          "s"
        case BaseT("bool") =>
          refFunctions += printBoolFunc
          "b"
        case _ => "error"
      }
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of print") ::
      expLines ++
      List(
        Comment("Print Logic"),
        Mov(R0, dest),
        BlInstr(s"_print${_type}")
      )
    }

    def printlnGenerate(exp: Expr): List[AssemblyLine] = {
      refFunctions += printLnFunc
      printGenerate(exp) ++
      List(BlInstr("_println"))
    }

    def arrayLiterGenerate(elems: List[Expr]): List[AssemblyLine] = {
      val pointer = allocator.allocateRegister(None)
      val arrayLines = new ListBuffer[AssemblyLine]()
      var totalSize = 4
      for (elem <- elems) {
        val next = allocator.allocateRegister(None)
        val elemLines = generateAssembly(elem, allocator, next)
        val size = getSize(elem)
        arrayLines ++= elemLines
        arrayLines ++= List(
          Mov(dest, next), 
          StoreInstr(dest, pointer, ImmVal(totalSize - 4))
        )
        totalSize += size
        allocator.deallocateRegister(next)
      }
      allocator.deallocateRegister(pointer)
      Comment("Start of array literal") ::
      List(
        Mov(R0, ImmVal(totalSize)),
        BlInstr("_malloc"),
        Mov(pointer, R0),
        AddInstr(pointer, pointer, ImmVal(totalSize - 4))
      ) ++
      arrayLines.toList ++
      List(
        Mov(dest, ImmVal(elems.length)),
        StoreInstr(dest, pointer, ImmVal(-4)),
        Mov(dest, pointer)
      )
    }

    def getSize(expr: Expr): Int = {
      expr.getType match {
        case BaseT("int") => 4
        case BaseT("char") => 1
        case BaseT("bool") => 1
        case BaseT("string") => 4
        case ArrayT(_, _) => 4
        case PairT(_, _) => 4
        case _ => 0
      }
    }

    def newPairGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val next = allocator.allocateRegister(None)
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of new pair") ::
      exp1Lines ++
      exp2Lines ++
      List(Comment("NewPair Logic"))
    }

    def pairElemGenerate(lvalue: LValue): List[AssemblyLine] = {
      val lvalueLines = generateAssembly(lvalue, allocator, dest)
      Comment("Start of pair element") ::
      lvalueLines ++
      List(Comment("PairElem Logic"))
    }

    def mulGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val next = allocator.allocateRegister(None)
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of multiplication") ::
      exp1Lines ++
      exp2Lines ++
      List(MulInstr(dest, dest, next))
    }

    def divGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val next = allocator.allocateRegister(None)
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of division") ::
      exp1Lines ++
      exp2Lines ++
      List(
        Comment("Div Logic"),
        Mov(R0, dest),
        Mov(R1, next),
        BlInstr("__aeabi_idivmod"),
        Mov(dest, R0)
      )
    }

    def modGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val next = allocator.allocateRegister(None)
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of modulo") ::
      exp1Lines ++
      exp2Lines ++
      List(
        Comment("Mod Logic"),
        Mov(R0, dest),
        Mov(R1, next),
        BlInstr("__aeabi_idivmod"),
        Mov(dest, R1)
      )
    }

    def addGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val next = allocator.allocateRegister(None)
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of addition") ::
      exp1Lines ++
      exp2Lines ++
      List(AddInstr(dest, dest, next))
    }

    def subGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val next = allocator.allocateRegister(None)
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of subtraction") ::
      exp1Lines ++
      exp2Lines ++
      List(SubInstr(dest, dest, next))
    }

    def gtGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val next = allocator.allocateRegister(None)
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of greater than") ::
      exp1Lines ++
      exp2Lines ++
      List(
        Comment("GT Logic"),
        CmpInstr(dest, next),
        Mov(dest, ImmVal(0)),
        Movgt(dest, ImmVal(1))
      )
    }

    def gteqGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val next = allocator.allocateRegister(None)
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of greater than or equal to") ::
      exp1Lines ++
      exp2Lines ++
      List(
        Comment("GTEQ Logic"),
        CmpInstr(dest, next),
        Mov(dest, ImmVal(0)),
        Movge(dest, ImmVal(1))
      )
    }

    def ltGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val next = allocator.allocateRegister(None)
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of less than") ::
      exp1Lines ++
      exp2Lines ++
      List(
        Comment("LT Logic"),
        CmpInstr(dest, next),
        Mov(dest, ImmVal(0)),
        Movlt(dest, ImmVal(1))
      )
    }

    def lteqGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val next = allocator.allocateRegister(None)
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of less than or equal to") ::
      exp1Lines ++
      exp2Lines ++
      List(
        Comment("LTEQ Logic"),
        CmpInstr(dest, next),
        Mov(dest, ImmVal(0)),
        Movle(dest, ImmVal(1))
      )
    }

    def eqLogic(dest: Register, next: Register): List[AssemblyLine] = {
      List(
        Comment("EQ Logic"),
        CmpInstr(dest, next),
        Mov(dest, ImmVal(0)),
        Moveq(dest, ImmVal(1))
      )
    }

    def eqGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val next = allocator.allocateRegister(None)
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of equality") ::
      exp1Lines ++
      exp2Lines ++
      eqLogic(dest, next)
    }

    def neqGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val next = allocator.allocateRegister(None)
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of not equal to") ::
      exp1Lines ++
      exp2Lines ++
      eqLogic(dest, next) ++
      notLogic(dest)
    }

    def andLogic(dest: Register, next: Register): List[AssemblyLine] = {
      List(
        Comment("and Logic"),
        CmpInstr(next, ImmVal(0)),
        Moveq(dest, ImmVal(0)),
      )
    }

    def andGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val next = allocator.allocateRegister(None)
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of and") ::
      exp1Lines ++
      exp2Lines ++
      andLogic(dest, next)
    }

    def orLogic(dest: Register, next: Register): List[AssemblyLine] = {
      List(
        Comment("or Logic"),
        CmpInstr(next, ImmVal(1)),
        Moveq(dest, ImmVal(1)),
      )
    }

    def orGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val next = allocator.allocateRegister(None)
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of or") ::
      exp1Lines ++
      exp2Lines ++
      orLogic(dest, next)
    }

    def notLogic(dest: Register): List[AssemblyLine] = {
      List(
        Comment("not Logic"),
        CmpInstr(dest, ImmVal(0)),
        Movne(dest, ImmVal(0)),
        Moveq(dest, ImmVal(1))
      )
    }

    def notGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of not") ::
      expLines ++
      notLogic(dest)
    }

    def negGenerate(exp: Expr): List[AssemblyLine] = {
      val tempReg = allocator.allocateRegister(None)
      val expLines = generateAssembly(exp, allocator, tempReg)
      allocator.deallocateRegister(tempReg)
      Comment("Start of negation") ::
      expLines ++
      List(
        Comment("neg Logic"),
        Mov(dest, ImmVal(0)),
        SubInstr(dest, dest, tempReg)
      )
    }

    def lenGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of length") ::
      expLines ++
      List(Comment("len Logic"))
    }

    def ordGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of ord") ::
      expLines ++
      List(Comment("ord Logic"))
    }

    def chrGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of chr") ::
      expLines ++
      List(Comment("chr Logic"))
    }

    def numGenerate(n: Int): List[AssemblyLine] = {
      List(
        Comment("Start of number"),
        Mov(dest, ImmVal(n))
      )
    }

    def boolGenerate(b: String): List[AssemblyLine] = {
      Comment("Start of boolean") ::
      (b match {
        case "true" =>
          List(
            Mov(dest, ImmVal(1))
          )
        case "false" =>
          List(
            Mov(dest, ImmVal(0))
          )
      })
    }

    def chGenerate(c: Char): List[AssemblyLine] = {
      List(
        Comment("Start of character"),
        Mov(dest, ImmVal(c.toInt))
      )
    }

    def identGenerate(n: String): List[AssemblyLine] = {
      val reg: Option[Register] = allocator.lookupRegister(n)
      Comment("Start of identifier") ::
      (reg match {
        case Some(r) => { 
          allocator.changeRegister(n, dest)
          List(
            Mov(dest, r)
          )
        }
        case None => {
          allocator.changeRegister(n, dest)
          List()
        }
      })
    }

    def strGenerate(s: String): List[AssemblyLine] = {
      val label = getStringLabel
      val asciz = AscizInstr(label, s)
      stringPool += asciz
      Comment("Start of string") ::
      List(
        LdrLabel(dest, LabelAddr(label))
      )
    }

    def arrayElemGenerate(id: Ident, indices: List[Expr]) = {
      val idLines = generateAssembly(id, allocator, dest)
      val indicesLines = new ListBuffer[AssemblyLine]()
      // Must check size of array elements and call different _arrLoad function
      for (index <- indices) {
        val next = allocator.allocateRegister(None)
        val indexLines = generateAssembly(index, allocator, next)
        indicesLines ++= indexLines
        indicesLines ++= List(
          Mov(R10, next),
          Mov(R3, dest),
          BlInstr("_arrLoad4"),
          Mov(dest, R3)
        )
        allocator.deallocateRegister(next)
      }
      Comment("Start of array element") ::
      idLines ++
      indicesLines.toList
    }

    def callGenerate(funcName: Ident, args: List[Expr]): List[AssemblyLine] = {
      Comment("Start of function call") ::
      argsGenerate(args) ++
      List(
        Comment("Call Logic"),
        BlInstr(funcName.nickname.get)
      )
    }

    def argsGenerate(args: List[Expr]): List[AssemblyLine] = {
      var argsLines = new ListBuffer[AssemblyLine]()
      var regNum = 0
      for (arg <- args) {
        val next = allocator.allocateRegister(None)
        val argLines = generateAssembly(arg, allocator, next)
        argsLines ++= argLines
        allocator.deallocateRegister(next)
        regNum match {
          case 0 => {
            argsLines += Mov(R0, next)
            regNum += 1
          }
          case 1 => {
            argsLines += Mov(R1, next)
            regNum += 1
          }
          case 2 => {
            argsLines += Mov(R2, next)
            regNum += 1
          }
          case 3 => {
            argsLines += Mov(R3, next)
            regNum += 1
          }
          case _ => argsLines += Push(next)
        }
      }
      argsLines.toList
    }

    ast match {
      case Program(funcs, stmts) =>
        programGenerate(funcs, stmts)

      case Function(_, funcName, paramList, body) =>
        functionGenerate(funcName, paramList, body)

      case Skip() =>
        List(Comment("Skip"))

      case Declare(_, id, value) =>
        declareGenerate(id, value)

      case Assign(lvalue, rvalue) =>
        assignGenerate(lvalue, rvalue)

      case Read(lvalue) =>
        readGenerate(lvalue)

      case If(cond, thenS, elseS) =>
        ifGenerate(cond, thenS, elseS)

      case While(cond, stmt) =>
        whileGenerate(cond, stmt)

      case Scope(body) =>
        scopeGenerate(body)

      case Statements(stmts) =>
        val stmtLines = stmts.flatMap(generateAssembly(_, allocator, dest))
        stmtLines

      case Free(exp) =>
        freeGenerate(exp)

      case Return(exp) =>
        returnGenerate(exp)

      case Exit(exp) =>
        exitGenerate(exp)

      case Print(exp) =>
        printGenerate(exp)

      case Println(exp) =>
        printlnGenerate(exp)

      case ArrayLiter(elems) =>
        arrayLiterGenerate(elems)

      case NewPair(exp1, exp2) =>
        newPairGenerate(exp1, exp2)

      case PairElem(_, lvalue) =>
        pairElemGenerate(lvalue)

      case Call(funcName, args) =>
        callGenerate(funcName, args)

      case x: BinOp =>
        x match {
          case Mul(exp1, exp2) =>
            mulGenerate(exp1, exp2)

          case Div(exp1, exp2) =>
            divGenerate(exp1, exp2)

          case Mod(exp1, exp2) =>
            modGenerate(exp1, exp2)

          case Add(exp1, exp2) =>
            addGenerate(exp1, exp2)

          case Sub(exp1, exp2) =>
            subGenerate(exp1, exp2)
        }

      case x: BinOpCompare =>
        x match {
          case GT(exp1, exp2) =>
            gtGenerate(exp1, exp2)

          case GTEQ(exp1, exp2) =>
            gteqGenerate(exp1, exp2)

          case LT(exp1, exp2) =>
            ltGenerate(exp1, exp2)

          case LTEQ(exp1, exp2) =>
            lteqGenerate(exp1, exp2)
        }

      case x: Equality =>
        x match {
          case EQ(exp1, exp2) =>
            eqGenerate(exp1, exp2)

          case NEQ(exp1, exp2) =>
            neqGenerate(exp1, exp2)
        }

      case x: BinOpLogic =>
        x match {
          case And(exp1, exp2) =>
            andGenerate(exp1, exp2)

          case Or(exp1, exp2) =>
            orGenerate(exp1, exp2)
        }

      case Not(exp) =>
        notGenerate(exp)

      case Neg(exp) =>
        negGenerate(exp)

      case Len(exp) =>
        lenGenerate(exp)

      case Ord(exp) =>
        ordGenerate(exp)

      case Chr(exp) =>
        chrGenerate(exp)

      case Num(n) =>
        numGenerate(n)

      case Bool(b) =>
        boolGenerate(b)

      case Ch(c) =>
        chGenerate(c)

      case Str(s) =>
        strGenerate(s)

      case PairLiter(_) =>
        List(
          Comment("Start of pair literal"),
          Mov(dest, ImmVal(0))
        )

      case Ident(_, n) =>
        identGenerate(n.get)

      case ArrayElem(f, p) =>
        arrayElemGenerate(f, p)

      case _ => List()
    }
  }
}
