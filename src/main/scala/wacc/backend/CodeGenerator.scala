package wacc.backend

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import wacc.ASTNodes._
import wacc.backend.ReferenceFunctions._
import wacc.backend.ARMAssemblyPrinter._

/* Generates ARM assembly code from an AST */
object CodeGenerator {
  private var labelCounter: Int = -1
  private var stringCounter: Int = -1
  val refFunctions: mutable.Set[List[Instruction]] = mutable.Set()
  private val stringPool: mutable.Set[AscizInstr] = mutable.Set()

  /* Generates a unique label for a String literal */
  private def getStringLabel: String = {
    stringCounter += 1
    ".L._str" + stringCounter
  }

  private def getUniqueLabel: String = {
    labelCounter += 1
    ".L" + labelCounter
  }

  def generateAssembly(ast: ASTNode, allocator: BasicRegisterAllocator, dest: Register): List[Instruction] = {

    def programGenerate(funcs: List[Function], stmts: Statement): List[Instruction] = {
      val stmtLines = generateAssembly(stmts, allocator, dest)
      val funcLines = funcs.flatMap(generateAssembly(_, allocator, dest))
      List(
        Comment("Start of program"),
        Command("data", 0)
      ) ++
      stringPool.toList ++
      List(
        Command("align 4", 0),
        Command("text", 0),
        Command("global main", 0),
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
      refFunctions.foldLeft(List[Instruction]())(_ ++ _)
    }

    def functionGenerate(_type: Type, funcName: Ident, params: List[Param], body: Statement): List[Instruction] = {
      allocator.setLocation(funcName.nickname.get, VariableLocation(R0, 0, 4, _type))
      Comment("Start of function") ::
      Label(funcName.nickname.get) ::
      List(
        Push(List(FP, LR)),
        Push(List(R4, R5, R6, R7, R8, R9, R10)),
        Mov(FP, SP)
      ) ++
      paramsGenerate(params) ++
      (body match {
        case Statements(stmts) => 
          stmts.flatMap(
            Push(List(R0, R1, R2, R3)) :: 
            generateAssembly(_, allocator, dest) ++ 
            List(Pop(List(R0, R1, R2, R3))))
        case _ => {
          Push(List(R0, R1, R2, R3)) ::
          generateAssembly(body, allocator, dest) ++
          List(Pop(List(R0, R1, R2, R3)))
        }}) ++
      List(
        Command("ltorg", 4)
      )
    }

    def paramsGenerate(params: List[Param]): List[Instruction] = {
      val paramLines = new ListBuffer[Instruction]()
      for (i <- params.indices) {
        val param = params(i)
        val (next, rLines) = allocator.allocateRegister()
        i match {
          case 0 =>
            paramLines ++= rLines
            paramLines += Mov(next, R0)
            allocator.setLocation(param.ident.nickname.get, VariableLocation(next, 0, 4, param._type))
          case 1 =>
            paramLines ++= rLines
            paramLines += Mov(next, R1)
            allocator.setLocation(param.ident.nickname.get, VariableLocation(next, 0, 4, param._type))
          case 2 =>
            paramLines ++= rLines
            paramLines += Mov(next, R2)
            allocator.setLocation(param.ident.nickname.get, VariableLocation(next, 0, 4, param._type))
          case 3 =>
            paramLines ++= rLines
            paramLines += Mov(next, R3)
            allocator.setLocation(param.ident.nickname.get, VariableLocation(next, 0, 4, param._type))
          case _ =>
            allocator.deallocateRegister(next)
            allocator.setLocation(param.ident.nickname.get, VariableLocation(FP, 4 * (params.length - i - 1), 4, param._type))
        }
      }
      paramLines.toList
    }

    def declareGenerate(_type: Type, id: Ident, value: RValue): List[Instruction] = {
      val (newDest, rLines) = allocator.allocateRegister()
      allocator.setLocation(id.nickname.get, VariableLocation(newDest, 0, 4, _type))
      val idLines = generateAssembly(id, allocator, newDest)
      val valueLines = generateAssembly(value, allocator, newDest)
      Comment("Start of declare") ::
      rLines ++
      idLines ++
      valueLines
    }

    def assignGenerate(lvalue: LValue, rvalue: RValue): List[Instruction] = {
      val rvalueLines = generateAssembly(rvalue, allocator, dest)
      val (beforeLines, afterLines, target) = getLvalueLocation(lvalue)
      Comment("Start of assign") ::
      rvalueLines ++
      beforeLines ++
      List(Mov(target, dest)) ++
      afterLines
    }

    def getLvalueLocation(lvalue: LValue): (List[Instruction], List[Instruction], Register) = {
      lvalue match {
        case Ident(_, nickname, _) =>
          val identLoc: VariableLocation = allocator.lookupLocation(nickname.get).get
          (List(), List(), identLoc.register)
        case ArrayElem(ident, indices) =>
          val identLoc: VariableLocation = allocator.lookupLocation(ident.nickname.get).get
          val (before, after, target) = getArrayElemLocation(identLoc._type, identLoc.register, indices)
          (before, after, target)
        case PairElem(func, lvalue) =>
          refFunctions += errorNullFunc
          val (newDest, rLines) = allocator.allocateRegister()
          func match {
            case "fst" =>
              val (beforeLines, afterLines, lvalueLoc) = getLvalueLocation(lvalue)
              allocator.deallocateRegister(newDest)
              (beforeLines ++
              pairLines(lvalue, lvalueLoc) ++
              rLines ++
              List(
                Ldr(newDest, Addr(lvalueLoc, ImmVal(0)))
              ), 
              pairLines(lvalue, lvalueLoc) ++
              List(
                StrInstr(newDest, Addr(lvalueLoc, ImmVal(0)))
              ) ++ 
              afterLines, 
              newDest)
            case "snd" =>
              val (beforeLines, afterLines, lvalueLoc) = getLvalueLocation(lvalue)
              allocator.deallocateRegister(newDest)
              (pairLines(lvalue, lvalueLoc) ++
              rLines ++
              List(
                Ldr(newDest, Addr(lvalueLoc, ImmVal(4)))
              ) ++
              beforeLines, 
              afterLines ++
              pairLines(lvalue, lvalueLoc) ++
              List(
                StrInstr(newDest, Addr(lvalueLoc, ImmVal(4)))
              ), 
              newDest)
          }
      }
    }

    def pairLines(lvalue: LValue, loc: Register): List[Instruction] = {
      lvalue.getType match {
        case PairT(_, _) => List(
          CmpInstr(loc, ImmVal(0)),
          BInstr("_errNull", EQcond),
        )
        case _ => List()
      }
    }

    def getArrayElemLocation(arrayType: Type, arrayReg: Register, indices: List[Expr]): (List[Instruction], List[Instruction], Register) = {
      refFunctions += arrayLoad4Func
      if (indices.isEmpty) {
        return (List(), List(), arrayReg)
      }
      val beforeLines = new ListBuffer[Instruction]()
      val afterLines = new ListBuffer[Instruction]()
      val (indexReg, r1Lines) = allocator.allocateRegister()
      val (elemReg, r2Lines) = allocator.allocateRegister()

      if (indexReg == R0 || indexReg == R3 || indexReg == R8) {
        val (indexReg, r1Lines) = allocator.allocateRegister()
      }
      if (elemReg == R0 || elemReg == R3 || elemReg == R8) {
        val (elemReg, r2Lines) = allocator.allocateRegister()
      }

      val indexLines = generateAssembly(indices.head, allocator, indexReg)
      beforeLines ++= r1Lines
      beforeLines ++= r2Lines
      beforeLines ++= indexLines
      beforeLines ++= List(
        Mov(R0, indexReg),
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
        Mov(R0, indexReg),
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

    def readGenerate(lvalue: LValue): List[Instruction] = {
      val _type = lvalue.getType match {
        case BaseT("int") =>
          refFunctions += readIntFunc
          "i"
        case BaseT("char") =>
          refFunctions += readCharFunc
          "c"
        case _ => ""
      }
      val (beforeLines, afterLines, target): (List[Instruction], List[Instruction], Register) = getLvalueLocation(lvalue)
      Comment("Start of read") ::
      beforeLines ++
      List(
        Mov(dest, target),
        Mov(R0, dest),
        BlInstr(s"_read${_type}"),
        Mov(target, R0)
      ) ++
      afterLines
    }

    def ifGenerate(cond: Expr, thenS: Statement, elseS: Statement): List[Instruction] = {
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

    def whileGenerate(cond: Expr, stmt: Statement): List[Instruction] = {
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

    def scopeGenerate(body: Statement): List[Instruction] = {
      val bodyLines = generateAssembly(body, allocator, dest)
      Comment("Start of new scope") ::
      bodyLines ++
      List(Comment("End of new scope"))
    }

    def freeGenerate(exp: Expr): List[Instruction] = {
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

    def returnGenerate(exp: Expr): List[Instruction] = {
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of return") ::
      expLines ++
      List(
        Comment("Return Logic"),
        Mov(R0, dest),
        Mov(SP, FP),
        Pop(List(R4, R5, R6, R7, R8, R9, R10)),
        Pop(List(FP, PC))
      )
    }

    def exitGenerate(exp: Expr): List[Instruction] = {
      refFunctions += exitFunc
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of exit") ::
      expLines ++
      List(
        Comment("Exit Logic"),
        Mov(R0, dest),
        BlInstr("_exit"),
        Mov(SP, FP),
        Pop(List(FP, PC))
      )
    }

    def printGenerate(exp: Expr): List[Instruction] = {
      var _type: Type = exp.getType
      print(exp)
      print(exp.getType)
      exp match {
        case Ident(_, _, t) =>
          t match {
            case Some(x) => _type = x
            case None => BaseT("ERROR")
          }
        case ArrayElem(ident, indices) =>
          ident._type match {
            case Some(t) =>
              t match {
                case ArrayT(t, n) if n > indices.length => _type = ArrayT(t, n - indices.length)
                case ArrayT(t, n) if n == indices.length => _type = t
                case _ => _type = BaseT("ERROR")
              }
            case None => _type = BaseT("ERROR")
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
        BlInstr(s"_print$t")
      )
    }

    def printlnGenerate(exp: Expr): List[Instruction] = {
      refFunctions += printLnFunc
      printGenerate(exp) ++
      List(BlInstr("_println"))
    }

    def arrayLiterGenerate(elems: List[Expr]): List[Instruction] = {
      refFunctions += mallocFunc
      val (pointer, r1Lines) = allocator.allocateRegister()
      val arrayLines = new ListBuffer[Instruction]()
      var totalSize = 0
      for (elem <- elems) {
        val (next, r2Lines) = allocator.allocateRegister()
        val elemLines = generateAssembly(elem, allocator, next)
        val size = getSize(elem)
        val sizeToStore = size match {
          case 1 => OneByte
          case _ => FourBytes
        }
        totalSize += size
        arrayLines ++= r2Lines
        arrayLines ++= elemLines
        arrayLines ++= List(
          Mov(dest, next), 
          StrInstr(dest, Addr(pointer, ImmVal(totalSize - size)), sizeToStore)
        )
        allocator.deallocateRegister(next)
      }
      totalSize += 4
      allocator.deallocateRegister(pointer)
      Comment("Start of array literal") ::
      r1Lines ++
      List(
        Mov(R0, ImmVal(totalSize)),
        BlInstr("_malloc"),
        Mov(pointer, R0),
        AddInstr(pointer, pointer, ImmVal(4)),
        Mov(dest, ImmVal(elems.length)),
        StrInstr(dest, Addr(pointer, ImmVal(-4))),
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

    def newPairGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
      refFunctions += mallocFunc
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
        StrInstr(next, Addr(dest, ImmVal(0)))
      ) ++
      generateAssembly(exp2, allocator, next) ++
      List(
        StrInstr(next, Addr(dest, ImmVal(4))),
        Mov(dest, dest)
      )
      allocator.deallocateRegister(next)
      newpairLines
    }

    def pairElemGenerate(func: String, lvalue: LValue): List[Instruction] = {
      refFunctions += errorNullFunc
      val (next, rLines) = allocator.allocateRegister()
      val lvalueLines = generateAssembly(lvalue, allocator, next)
      allocator.deallocateRegister(next)
      val funcLines = func match {
        case "fst" =>
          List(
            Ldr(dest, Addr(next, ImmVal(0)))
          )
        case "snd" =>
          List(
            Ldr(dest, Addr(next, ImmVal(4)))
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

    def mulGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
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
        Pop(List(val1, val2).sortBy(_.number)),
        SmullInstr(dest, hi, val1, val2),
        CmpInstr(hi, dest, ShiftRight(31)),
        BlInstr("_errOverflow", NEcond)
      )
    }

    def divGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
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

    def modGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
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

    def addGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
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

    def subGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
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

    def condGenerate(exp1: Expr, exp2: Expr, cond: Condition): List[Instruction] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of conditional") ::
      exp1Lines ++
      rLines ++ 
      exp2Lines ++
      List(
        Comment("Conditional Logic"),
        CmpInstr(dest, next),
        Mov(dest, ImmVal(0)),
        Mov(dest, ImmVal(1), cond)
      ) ++
      List(Comment("End of conditional"))
    }

    def andGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of and") ::
      exp1Lines ++
      rLines ++ 
      exp2Lines ++
      List(
        Comment("and Logic"),
        CmpInstr(next, ImmVal(0)),
        Mov(dest, ImmVal(0), EQcond),
      )
    }

    def orGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
      val exp1Lines = generateAssembly(exp1, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      val exp2Lines = generateAssembly(exp2, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of or") ::
      exp1Lines ++
      rLines ++ 
      exp2Lines ++
      List(
        Comment("or Logic"),
        CmpInstr(next, ImmVal(1)),
        Mov(dest, ImmVal(1), EQcond),
      )
    }

    def notGenerate(exp: Expr): List[Instruction] = {
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of not") ::
      expLines ++
      List(
        Comment("not Logic"),
        CmpInstr(dest, ImmVal(0)),
        Mov(dest, ImmVal(0), NEcond),
        Mov(dest, ImmVal(1), EQcond)
      )
    }

    def negGenerate(exp: Expr): List[Instruction] = {
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

    def lenGenerate(exp: Expr): List[Instruction] = {
      val (next, rLines) = allocator.allocateRegister()
      val expLines = generateAssembly(exp, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of length") ::
      rLines ++ 
      expLines ++
      List(
        Comment("len Logic"),
        Ldr(dest, Addr(next, ImmVal(-4)))
      )
    }

    def ordGenerate(exp: Expr): List[Instruction] = {
      val expLines = generateAssembly(exp, allocator, dest)
      Comment("Start of ord") ::
      expLines ++
      List(Comment("ord Logic"))
    }

    def chrGenerate(exp: Expr): List[Instruction] = {
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

    def numGenerate(n: Int): List[Instruction] = {
      val instr = if (n < 0 || n > 255) {
        Ldr(dest, IntLiteral(n))
      } else {
        Mov(dest, ImmVal(n))
      }

      List(
        Comment("Start of number"),
        instr
      )
    }

    def boolGenerate(b: String): List[Instruction] = {
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

    def chGenerate(c: Char): List[Instruction] = {
      List(
        Comment("Start of character"),
        Mov(dest, ImmVal(c.toInt))
      )
    }

    def identGenerate(n: String): List[Instruction] = {
      val location: Option[VariableLocation] = allocator.lookupLocation(n)

      println("location of: " + n + " is: " + location)

      Comment("Start of identifier " + n) ::
      (location match {
        case Some(VariableLocation(reg, off, size, _type)) =>
          reg match {
            case FP =>
              val (next, rLines) = allocator.allocateRegister()
              allocator.setLocation(n, VariableLocation(next, 0, size, _type))
              rLines ++ List(
                Ldr(next, Addr(reg, ImmVal(off))),
                Mov(dest, next)
              )
            case _ => List(Mov(dest, reg))
          }
        case None => List()
      }) ++ List(Comment("End of identifier"))
    }

    def strGenerate(s: String): List[Instruction] = {
      val label = getStringLabel
      val asciz = AscizInstr(label, s)
      stringPool += asciz
      Comment("Start of string") ::
      List(
        Ldr(dest, LabelAddr(label))
      )
    }

    def arrayElemGenerate(id: Ident, indices: List[Expr]) = {
      refFunctions += arrayLoad4Func
      val idLines = generateAssembly(id, allocator, dest)
      val indicesLines = new ListBuffer[Instruction]()
      for (index <- indices) {
        val (next, rLines) = allocator.allocateRegister()
        if (next == R0 || next == R3 || next == R8) {
          val (next, rLines) = allocator.allocateRegister()
        }
        val indexLines = generateAssembly(index, allocator, next)
        indicesLines ++= rLines
        indicesLines ++= indexLines
        indicesLines ++= List(
          Mov(R0, next),
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

    def callGenerate(funcName: Ident, args: List[Expr]): List[Instruction] = {
      Comment("Start of function call") ::
      Push(List(R1, R2, R3)) ::
      argsGenerate(args) ++
      List(
        Comment("Call Logic"),
        BlInstr(funcName.nickname.get),
        Pop(List(R1, R2, R3)),
        Mov(dest, R0)
      )
    }

    def argsGenerate(args: List[Expr]): List[Instruction] = {
      val argsLines = new ListBuffer[Instruction]()
      for (i <- args.indices) {
        val arg = args(i)
        val (next, rLines) = allocator.allocateRegister()
        val argLines = generateAssembly(arg, allocator, next)
        argsLines ++= rLines
        argsLines ++= argLines
        allocator.deallocateRegister(next)
        i match {
          case 0 =>
            argsLines += Mov(R0, next)
          case 1 =>
            argsLines += Mov(R1, next)
          case 2 =>
            argsLines += Mov(R2, next)
          case 3 =>
            argsLines += Mov(R3, next)
          case _ => argsLines += StrInstr(next, Addr(SP, ImmVal(4 * (args.length - i - 1))))
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
            condGenerate(exp1, exp2, GTcond)

          case GTEQ(exp1, exp2) =>
            condGenerate(exp1, exp2, GEcond)

          case LT(exp1, exp2) =>
            condGenerate(exp1, exp2, LTcond)

          case LTEQ(exp1, exp2) =>
            condGenerate(exp1, exp2, LEcond)
        }

      case x: Equality =>
        x match {
          case EQ(exp1, exp2) =>
            condGenerate(exp1, exp2, EQcond)

          case NEQ(exp1, exp2) =>
            condGenerate(exp1, exp2, NEcond)
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

      case Ident(s, n, _) =>
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
