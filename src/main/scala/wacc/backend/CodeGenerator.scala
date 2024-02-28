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
    Push(List(FP, LR)),
    Mov(FP, SP),
    BicInstr(SP, SP, ImmVal(7)),
    BlInstr("exit"),
    Mov(SP, FP),
    Pop(List(FP, PC))
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

  private lazy val printStrFunc: List[AssemblyLine] = {
    List(
      NewLine(),
      AscizInstr(".L._prints_str0", "%.*s"),
      Command("align 4"),
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

  private lazy val printPairFunc: List[AssemblyLine] = {
    List(
      NewLine(),
      AscizInstr(".L._printp_str0", "%p"),
      Command("align 4"),
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

  private lazy val printBoolFunc: List[AssemblyLine] = {
    List(
      NewLine(),
      AscizInstr(".L._printb_str0", "false"),
      AscizInstr(".L._printb_str1", "true"),
      AscizInstr(".L._printb_str2", "%.*s"),
      Command("align 4"),
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

  private lazy val printLnFunc: List[AssemblyLine] = {
    List(
      NewLine(),
      AscizInstr(".L._println_str0", ""),
      Command("align 4"),
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

  private lazy val mallocFunc: List[AssemblyLine] = {
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

  private lazy val readIntFunc: List[AssemblyLine] = {
    List(
      NewLine(),
      AscizInstr(".L._readi_str0", "%d"),
      Command("align 4"),
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

  private lazy val readCharFunc: List[AssemblyLine] = {
    List(
      NewLine(),
      AscizInstr(".L._readc_str0", " %c"),
      Command("align 4"),
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

  private lazy val freeFunc: List[AssemblyLine] = List(
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

  private lazy val freePairFunc: List[AssemblyLine] = {
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

  private lazy val arrayLoad4Func: List[AssemblyLine] = {
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

  private lazy val arrayStore4Func: List[AssemblyLine] = {
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

  private lazy val errorOutOfMemoryFunc: List[AssemblyLine] = {
    refFunctions += printStrFunc
    List(
      NewLine(),
      Comment("Error out of memory function"),
      AscizInstr(".L._errOutOfMemory_str0", "Error: Out of memory"),
      Command("align 4"),
      Label("_errOutOfMemory"),
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errOutOfMemory_str0"),
      BlInstr("_prints"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit"),
    )
  }

  private lazy val errorOutOfBoundsFunc: List[AssemblyLine] = List(
    NewLine(),
    Comment("Error out of bounds function"),
    AscizInstr(".L._errOutOfBounds_str0", "Error: Array index out of bounds"),
    Command("align 4"),
    Label("_errOutOfBounds"),
    BicInstr(SP, SP, ImmVal(7)),
    AdrInstr(R0, ".L._errOutOfBounds_str0"),
    BlInstr("printf"),
    Mov(R0, ImmVal(0)),
    BlInstr("fflush"),
    Mov(R0, ImmVal(255)),
    BlInstr("exit")
  )

  private lazy val errorNullFunc: List[AssemblyLine] = {
    refFunctions += printStrFunc
    List(
      NewLine(),
      Comment("Error null pointer function"),
      AscizInstr(".L._errNull_str0", "Error: Null pair dereferenced"),
      Command("align 4"),
      Label("_errNull"),
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errNull_str0"),
      BlInstr("_prints"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit")
    )
  }

  private lazy val errorOverflowFunc: List[AssemblyLine] = {
    refFunctions += printStrFunc
    List(
      NewLine(),
      Comment("Error overflow function"),
      AscizInstr(".L._errOverflow_str0", "Error: Integer overflow or underflow occured"),
      Command("align 4"),
      Label("_errOverflow"),
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errOverflow_str0"),
      BlInstr("_prints"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit")
    )
  }

  private lazy val errorDivByZeroFunc: List[AssemblyLine] = {
    refFunctions += printStrFunc
    List(
      NewLine(),
      Comment("Error division by zero function"),
      AscizInstr(".L._errDivZero_str0", "Error: Division by zero"),
      Command("align 4"),
      Label("_errDivZero"),
      BicInstr(SP, SP, ImmVal(7)),
      AdrInstr(R0, ".L._errDivZero_str0"),
      BlInstr("_prints"),
      Mov(R0, ImmVal(255)),
      BlInstr("exit")
    )
  }

  private lazy val errorBadCharFunc: List[AssemblyLine] = {
    refFunctions += printStrFunc
    List(
      NewLine(),
      Comment("Error bad character function"),
      AscizInstr(".L._errBadChar_str0", "fatal error: int %d is not ascii character 0-127 \n"),
      Command("align 4"),
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

  private lazy val arrayStore1Func: List[AssemblyLine] = {
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
        Push(List(FP, LR)), 
        Mov(FP, SP)
      ) ++
      stmtLines ++
      List(
        Mov(R0, ImmVal(0)),
        Pop(List(FP, PC))
      ) ++
      funcLines ++
      refFunctions.foldLeft(List[AssemblyLine]())(_ ++ _)
    }

    def functionGenerate(_type: Type, funcName: Ident, params: List[Param], body: Statement): List[AssemblyLine] = {
      allocator.setLocation(funcName.nickname.get, VariableLocation(R0, 0, 4, _type))
      Comment("Start of function") ::
      Label(funcName.nickname.get) ::
      List(
        Push(List(FP, LR)),
        Mov(FP, SP)
      ) ++
      paramsGenerate(params) ++
      generateAssembly(body, allocator, dest) ++
      List(LtorgInstr())
    }

    def paramsGenerate(params: List[Param]): List[AssemblyLine] = {
      var paramLines = new ListBuffer[AssemblyLine]()
      var regNum = 0
      for (param <- params) {
        val (next, rLines) = allocator.allocateRegister()
        paramLines ++= rLines
        regNum match {
          case 0 => {
            paramLines += Mov(next, R0)
            allocator.setLocation(param.ident.nickname.get, VariableLocation(next, 0, 4, param._type))
            regNum += 1
          }
          case 1 => {
            paramLines += Mov(next, R1)
            allocator.setLocation(param.ident.nickname.get, VariableLocation(next, 0, 4, param._type))
            regNum += 1
          }
          case 2 => {
            paramLines += Mov(next, R2)
            allocator.setLocation(param.ident.nickname.get, VariableLocation(next, 0, 4, param._type))
            regNum += 1
          }
          case 3 => {
            paramLines += Mov(next, R3)
            allocator.setLocation(param.ident.nickname.get, VariableLocation(next, 0, 4, param._type))
            regNum += 1
          }
          case _ => {
            paramLines += Pop(List(next))
            allocator.setLocation(param.ident.nickname.get, VariableLocation(next, 0, 4, param._type))
          }
        }
      }
      paramLines.toList
    }

    def declareGenerate(_type: Type, id: Ident, value: RValue): List[AssemblyLine] = {
      val (newDest, rLines) = allocator.allocateRegister()
      allocator.setLocation(id.nickname.get, VariableLocation(newDest, 0, 4, _type))
      val idLines = generateAssembly(id, allocator, newDest)
      val valueLines = generateAssembly(value, allocator, newDest)
      value match {
        case Call(funcName, args) => 
          allocator.setLocation(id.nickname.get, VariableLocation(R0, 0, 4, _type))
        case _ =>
      }
      Comment("Start of declare") ::
      rLines ++
      idLines ++
      valueLines
    }

    def assignGenerate(lvalue: LValue, rvalue: RValue): List[AssemblyLine] = {
      val rvalueLines = generateAssembly(rvalue, allocator, dest)
      val (beforeLines, afterLines, _) = getLvalueLocation(lvalue)
      Comment("Start of assign") ::
      rvalueLines ++
      beforeLines ++
      afterLines
    }

    def getLvalueLocation(lvalue: LValue): (List[AssemblyLine], List[AssemblyLine], VariableLocation) = {
      lvalue match {
        case Ident(string, nickname, _type) => {
          val identLoc: VariableLocation = allocator.lookupLocation(nickname.get).get
          identLoc._type match {
            case ArrayT(_, _) => {
              (List(), List(StoreInstr(dest, identLoc.register, ImmVal(identLoc.offset))), identLoc)
            }
            case PairT(_, _) => {
              (List(), List(StoreInstr(dest, identLoc.register, ImmVal(identLoc.offset))), identLoc)
            }
            case _ => (List(Mov(identLoc.register, dest)), List(), identLoc)
          }
        }
        case ArrayElem(ident, indices) => {
          val identLoc: VariableLocation = allocator.lookupLocation(ident.nickname.get).get
          val (before, after, target) = getArrayElemLocation(identLoc._type, identLoc.register, indices)
          (before ++ List(Mov(target, dest)), after, identLoc)
        }
        case PairElem(func, lvalue) => {
          refFunctions += errorNullFunc
          func match {
            case "fst" => {
              val (beforeLines, afterLines, lvalueLoc) = getLvalueLocation(lvalue)
              (beforeLines ++
              pairLines(lvalue, lvalueLoc) ++
              List(
                LdrAddr(dest, lvalueLoc.register, ImmVal(lvalueLoc.offset))
              ), 
              pairLines(lvalue, lvalueLoc) ++
              List(
                StoreInstr(dest, lvalueLoc.register, ImmVal(lvalueLoc.offset))
              ) ++ 
              afterLines, 
              lvalueLoc)
            }
            case "snd" => {
              val (beforeLines, afterLines, lvalueLoc) = getLvalueLocation(lvalue)
              (pairLines(lvalue, lvalueLoc) ++
              List(
                LdrAddr(dest, lvalueLoc.register, ImmVal(lvalueLoc.offset + 4))
              ) ++
              beforeLines, 
              afterLines ++
              pairLines(lvalue, lvalueLoc) ++
              List(
                StoreInstr(dest, lvalueLoc.register, ImmVal(lvalueLoc.offset + 4))
              ), 
              lvalueLoc)
            }
          }
        }
      }
    }

    def pairLines(lvalue: LValue, loc: VariableLocation): List[AssemblyLine] = {
      lvalue.getType match {
        case PairT(_, _) => List(
          CmpInstr(loc.register, ImmVal(0)),
          BInstr("_errNull", EQcond),
        )
        case _ => List()
      }
    }

    def getArrayElemLocation(arrayType: Type, arrayReg: Register, indices: List[Expr]): (List[AssemblyLine], List[AssemblyLine], Register) = {
      refFunctions += arrayLoad4Func
      if (indices.isEmpty) {
        return (List(), List(), arrayReg)
      }
      val beforeLines = new ListBuffer[AssemblyLine]()
      val afterLines = new ListBuffer[AssemblyLine]()
      val (indexReg, r1Lines) = allocator.allocateRegister()
      val (elemReg, r2Lines) = allocator.allocateRegister()
      val indexLines = generateAssembly(indices.head, allocator, indexReg)
      beforeLines ++= r1Lines
      beforeLines ++= r2Lines
      beforeLines ++= indexLines
      beforeLines ++= List(
        Mov(R10, indexReg),
        Mov(R3, arrayReg),
        BlInstr("_arrLoad4"),
        Mov(elemReg, R3)
      )
      val (before, after, target) = getArrayElemLocation(reduceType(arrayType), elemReg, indices.tail)
      beforeLines ++= before
      val storeFunc = getTypeSize(reduceType(arrayType)) match {
        case 1 => 
          refFunctions += arrayStore1Func
          "_arrStore1"
        case _ => 
          refFunctions += arrayStore4Func
          "_arrStore4"
      }
      afterLines ++= after
      afterLines ++= List(
        Mov(R10, indexReg),
        Mov(R3, arrayReg),
        Mov(R8, elemReg),
        BlInstr(storeFunc),
        Mov(arrayReg, R3)
      )
      allocator.deallocateRegister(indexReg)
      allocator.deallocateRegister(elemReg)
      (beforeLines.toList, afterLines.toList, target)
    }

    def reduceType(t: Type): Type = {
      t match {
        case ArrayT(tp, d) if d > 1 => ArrayT(tp, d - 1)
        case ArrayT(tp, 1) => tp
        case _ => t
      }
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
      val (beforeLines, afterLines, target): (List[AssemblyLine], List[AssemblyLine], VariableLocation) = getLvalueLocation(lvalue)
      Comment("Start of read") ::
      beforeLines ++
      List(
        Mov(R0, target.register),
        BlInstr(s"_read${_type}"),
        Mov(target.register, R0)
      ) ++
      afterLines
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
        BInstr(elseLabel, NEcond)
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
      val (newDest, rLines) = allocator.allocateRegister()
      val stmtLines = generateAssembly(stmt, allocator, newDest)
      Comment("Start of while loop") ::
      Label(startLabel) ::
      condLines ++
      List(
        Comment("While loop condition logic"),
        CmpInstr(dest, ImmVal(1)), 
        BInstr(endLabel, NEcond)
      ) ++ 
      rLines ++
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
      val (_type, tLines) = exp.getType match {
        case PairT(_, _) => 
          refFunctions += freePairFunc
          ("pair", List())
        case ArrayT(_, _) => 
          ("", List(SubInstr(dest, dest, ImmVal(4))))
        case _ => ("", List())
      }
      Comment("Start of free") ::
      expLines ++
      tLines ++
      List(
        Mov(R0, dest),
        BlInstr("_free" + _type)
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
        Pop(List(FP, PC))
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
      var _type: Type = exp.getType
      exp match {
        case Ident(str, nickname, t) => {
          t match {
            case Some(x) => _type = x
            case None => BaseT("ERROR")
          }
        }
        case ArrayElem(ident, indices) => {
          ident._type match {
            case Some(t) => {
              t match {
                case ArrayT(t, n) if n > indices.length => _type = ArrayT(t, n - indices.length)
                case ArrayT(t, n) if n == indices.length => _type = t
                case _ => _type = BaseT("ERROR")
              }
            }
            case None => _type = BaseT("ERROR")
          }
        }
        case _ =>
      }

      val t = _type match {
        case BaseT("int") =>
          refFunctions += printCharOrIntFunc(false)
          "i"
        case BaseT("char") =>
          refFunctions += printCharOrIntFunc(true)
          "c"
        case PairLiter(_) =>
          refFunctions += printPairFunc
          "p"
        case BaseT("string") =>
          refFunctions += printStrFunc
          "s"
        case ArrayT(BaseT("char"), 1) =>
          refFunctions += printStrFunc
          "s"
        case BaseT("bool") =>
          refFunctions += printBoolFunc
          "b"
        case PairT(_, _) =>
          refFunctions += printPairFunc
          "p"
        case ArrayT(_, _) => 
          refFunctions += printPairFunc
          "p"
        case _ => "error"
      }
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of print") ::
      expLines ++
      List(
        Comment("Print Logic"),
        Mov(R0, dest),
        BlInstr(s"_print${t}")
      )
    }

    def printlnGenerate(exp: Expr): List[AssemblyLine] = {
      refFunctions += printLnFunc
      printGenerate(exp) ++
      List(BlInstr("_println"))
    }

    def arrayLiterGenerate(elems: List[Expr]): List[AssemblyLine] = {
      refFunctions += mallocFunc
      val (pointer, r1Lines) = allocator.allocateRegister()
      val arrayLines = new ListBuffer[AssemblyLine]()
      var totalSize = 4
      for (elem <- elems) {
        val (next, r2Lines) = allocator.allocateRegister()
        val elemLines = generateAssembly(elem, allocator, next)
        val size = getSize(elem)
        val sizeToStore = size match {
          case 1 => OneByte
          case _ => FourBytes
        }
        arrayLines ++= r2Lines
        arrayLines ++= elemLines
        arrayLines ++= List(
          Mov(dest, next), 
          StoreInstr(dest, pointer, ImmVal(totalSize - size), sizeToStore)
        )
        totalSize += size
        allocator.deallocateRegister(next)
      }
      allocator.deallocateRegister(pointer)
      Comment("Start of array literal") ::
      r1Lines ++
      List(
        Mov(R0, ImmVal(totalSize)),
        BlInstr("_malloc"),
        Mov(pointer, R0),
        AddInstr(pointer, pointer, ImmVal(4)),
        Mov(dest, ImmVal(elems.length)),
        StoreInstr(dest, pointer, ImmVal(-4)),
      ) ++
      arrayLines.toList ++
      List(
        Mov(dest, pointer)
      )
    }

    def getSize(expr: Expr): Int = {
      getTypeSize(expr.getType)
    }

    def getTypeSize(t: Type): Int = {
      t match {
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
      refFunctions += mallocFunc
      val size1 = getSize(exp1)
      val size2 = getSize(exp2)
      val (next, rLines) = allocator.allocateRegister()
      val newpairLines = Comment("Start of new pair") ::
      rLines ++
      List(
        Comment("NewPair Logic"),
        Mov(R0, ImmVal(8)),
        BlInstr("_malloc"),
        Mov(dest, R0),
      ) ++
      generateAssembly(exp1, allocator, next) ++
      List(
        StoreInstr(next, dest, ImmVal(0))
      ) ++
      generateAssembly(exp2, allocator, next) ++
      List(
        StoreInstr(next, dest, ImmVal(4)),
        Mov(dest, dest)
      )
      allocator.deallocateRegister(next)
      newpairLines
    }

    def pairElemGenerate(func: String, lvalue: LValue): List[AssemblyLine] = {
      refFunctions += errorNullFunc
      val (next, rLines) = allocator.allocateRegister()
      val lvalueLines = generateAssembly(lvalue, allocator, next)
      allocator.deallocateRegister(next)
      val funcLines = func match {
        case "fst" =>
          List(
            LdrAddr(dest, next, ImmVal(0))
          )
        case "snd" =>
          List(
            LdrAddr(dest, next, ImmVal(4))
          )
      }
      Comment("Start of pair element") ::
      rLines ++
      lvalueLines ++
      List(
        Comment("PairElem Logic"),
        CmpInstr(next, ImmVal(0)),
        BInstr("_errNull", EQcond)
      ) ++
      funcLines
    }

    def mulGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      refFunctions += errorOverflowFunc
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val exp2Lines = generateAssembly(exp2, allocator, dest)
      val (val1, r1Lines) = allocator.allocateRegister()
      val (val2, r2Lines) = allocator.allocateRegister()
      val (hi, r3Lines) = allocator.allocateRegister()
      allocator.deallocateRegister(hi)
      allocator.deallocateRegister(val1)
      allocator.deallocateRegister(val2)

      Comment("Start of multiplication") ::
      exp1Lines ++
      List(Push(List(dest))) ++
      exp2Lines ++
      List(Push(List(dest))) ++
      r1Lines ++
      r2Lines ++
      r3Lines ++
      List(
        Pop(List(val1, val2)),
        SmullInstr(dest, hi, val1, val2),
        CmpInstr(hi, dest, ShiftRight(31)),
        BlInstr("_errOverflow", NEcond)
      )
    }

    def divGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      refFunctions += errorDivByZeroFunc
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val exp2Lines = generateAssembly(exp2, allocator, dest)
      Comment("Start of division") ::
      exp1Lines ++
      List(Mov(R0, dest)) ++
      exp2Lines ++
      List(
        Mov(R1, dest),
        CmpInstr(R1, ImmVal(0)),
        BlInstr("_errDivZero", EQcond),
        BlInstr("__aeabi_idivmod"),
        Mov(dest, R0)
      )
    }

    def modGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      refFunctions += errorDivByZeroFunc
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val exp2Lines = generateAssembly(exp2, allocator, dest)
      Comment("Start of modulo") ::
      exp1Lines ++
      List(Mov(R0, dest)) ++
      exp2Lines ++
      List(
        Mov(R1, dest),
        CmpInstr(R1, ImmVal(0)),
        BlInstr("_errDivZero", EQcond),
        BlInstr("__aeabi_idivmod"),
        Mov(dest, R1)
      )
    }

    def addGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      refFunctions += errorOverflowFunc
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val exp2Lines = generateAssembly(exp2, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      allocator.deallocateRegister(next)
      Comment("Start of addition") ::
      exp2Lines ++
      List(Push(List(dest))) ++
      exp1Lines ++
      rLines ++ 
      List(
        Pop(List(next)),
        AddsInstr(dest, dest, next),
        BlInstr("_errOverflow", VScond)
      )
    }

    def subGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      refFunctions += errorOverflowFunc
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of subtraction") ::
      exp1Lines ++
      rLines ++ 
      exp2Lines ++
      List(
        SubsInstr(dest, dest, next),
        BlInstr("_errOverflow", VScond)
      )
    }

    def gtGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of greater than") ::
      exp1Lines ++
      rLines ++ 
      exp2Lines ++
      List(
        Comment("GT Logic"),
        CmpInstr(dest, next),
        Mov(dest, ImmVal(0)),
        Mov(dest, ImmVal(1), GTcond)
      )
    }

    def gteqGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of greater than or equal to") ::
      exp1Lines ++
      rLines ++ 
      exp2Lines ++
      List(
        Comment("GTEQ Logic"),
        CmpInstr(dest, next),
        Mov(dest, ImmVal(0)),
        Mov(dest, ImmVal(1), GEcond)
      )
    }

    def ltGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of less than") ::
      exp1Lines ++
      rLines ++ 
      exp2Lines ++
      List(
        Comment("LT Logic"),
        CmpInstr(dest, next),
        Mov(dest, ImmVal(0)),
        Mov(dest, ImmVal(1), LTcond)
      )
    }

    def lteqGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of less than or equal to") ::
      exp1Lines ++
      rLines ++ 
      exp2Lines ++
      List(
        Comment("LTEQ Logic"),
        CmpInstr(dest, next),
        Mov(dest, ImmVal(0)),
        Mov(dest, ImmVal(1), LEcond)
      )
    }

    def eqLogic(dest: Register, next: Register): List[AssemblyLine] = {
      List(
        Comment("EQ Logic"),
        CmpInstr(dest, next),
        Mov(dest, ImmVal(0)),
        Mov(dest, ImmVal(1), EQcond)
      )
    }

    def eqGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of equality") ::
      exp1Lines ++
      rLines ++ 
      exp2Lines ++
      eqLogic(dest, next)
    }

    def neqGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of not equal to") ::
      exp1Lines ++
      rLines ++ 
      exp2Lines ++
      eqLogic(dest, next) ++
      notLogic(dest)
    }

    def andLogic(dest: Register, next: Register): List[AssemblyLine] = {
      List(
        Comment("and Logic"),
        CmpInstr(next, ImmVal(0)),
        Mov(dest, ImmVal(0), EQcond),
      )
    }

    def andGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of and") ::
      exp1Lines ++
      rLines ++ 
      exp2Lines ++
      andLogic(dest, next)
    }

    def orLogic(dest: Register, next: Register): List[AssemblyLine] = {
      List(
        Comment("or Logic"),
        CmpInstr(next, ImmVal(1)),
        Mov(dest, ImmVal(1), EQcond),
      )
    }

    def orGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of or") ::
      exp1Lines ++
      rLines ++ 
      exp2Lines ++
      orLogic(dest, next)
    }

    def notLogic(dest: Register): List[AssemblyLine] = {
      List(
        Comment("not Logic"),
        CmpInstr(dest, ImmVal(0)),
        Mov(dest, ImmVal(0), NEcond),
        Mov(dest, ImmVal(1), EQcond)
      )
    }

    def notGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of not") ::
      expLines ++
      notLogic(dest)
    }

    def negGenerate(exp: Expr): List[AssemblyLine] = {
      refFunctions += errorOverflowFunc
      val (tempReg, rLines) = allocator.allocateRegister()
      val expLines = generateAssembly(exp, allocator, tempReg)
      allocator.deallocateRegister(tempReg)
      Comment("Start of negation") ::
      rLines ++ 
      expLines ++
      List(
        Comment("neg Logic"),
        RsbsInstr(dest, tempReg),
        BlInstr("_errOverflow", VScond)
      )
    }

    def lenGenerate(exp: Expr): List[AssemblyLine] = {
      val (next, rLines) = allocator.allocateRegister()
      val expLines = generateAssembly(exp, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of length") ::
      rLines ++ 
      expLines ++
      List(
        Comment("len Logic"),
        LdrAddr(dest, next, ImmVal(-4))
      )
    }

    def ordGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of ord") ::
      expLines ++
      List(Comment("ord Logic"))
    }

    def chrGenerate(exp: Expr): List[AssemblyLine] = {
      refFunctions += errorBadCharFunc
      val (next, rLines) = allocator.allocateRegister()
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of chr") ::
      expLines ++
      rLines ++
      List(
        Comment("chr Logic"),
        Mov(next, ImmVal(-128)),
        Tst(dest, next), 
        Mov(R1, dest, NEcond),
        BlInstr("_errBadChar", NEcond)
      )
    }

    def numGenerate(n: Int): List[AssemblyLine] = {
      val instr = if (n < 0 || n > 255) {
        LdrImm(dest, n)
      } else {
        Mov(dest, ImmVal(n))
      }

      List(
        Comment("Start of number"),
        instr
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
      val location: Option[VariableLocation] = allocator.lookupLocation(n)
      Comment("Start of identifier") ::
      (location match {
        case Some(VariableLocation(reg, off, size, _type)) => {
          reg match {
            case FP => {
              val (next, rLines) = allocator.allocateRegister()
              allocator.setLocation(n, VariableLocation(next, 0, size, _type))
              rLines ++ List(
                LdrAddr(next, reg, ImmVal(off)),
                Mov(dest, next)
              )
            }
            case _ => List(Mov(dest, reg))
          }
        }
        case None => List()
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
      refFunctions += arrayLoad4Func
      val idLines = generateAssembly(id, allocator, dest)
      val indicesLines = new ListBuffer[AssemblyLine]()
      // Must check size of array elements and call different _arrLoad function
      for (index <- indices) {
        val (next, rLines) = allocator.allocateRegister()
        val indexLines = generateAssembly(index, allocator, next)
        indicesLines ++= rLines
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
        val (next, rLines) = allocator.allocateRegister()
        val argLines = generateAssembly(arg, allocator, next)
        argsLines ++= rLines
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
          case _ => argsLines += Push(List(next))
        }
      }
      argsLines.toList
    }

    ast match {
      case Program(funcs, stmts) =>
        programGenerate(funcs, stmts)

      case Function(_type, funcName, paramList, body) =>
        functionGenerate(_type, funcName, paramList, body)

      case Skip() =>
        List(Comment("Skip"))

      case Declare(_type, id, value) =>
        declareGenerate(_type, id, value)

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

      case PairElem(func, lvalue) =>
        pairElemGenerate(func, lvalue)

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

      case Ident(s, n, t) =>
        n match {
          case Some(v) => identGenerate(v)
          case None => identGenerate(s)
        }

      case ArrayElem(f, p) =>
        arrayElemGenerate(f, p)

      case _ => List()
    }
  }
}
