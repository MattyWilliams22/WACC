package wacc.backend

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import wacc.ASTNodes._
import wacc.backend.PredefinedFunctions._

/* Used to generate instructions for an AST */
object CodeGenerator {

  /* Used to generate a unique label */
  private var labelCounter: Int = -1

  /* Used to generate a unique label for a string literal */
  private var stringCounter: Int = -1
  
  /* Used to store the predefined functions required by the program */
  val predefinedFunctions: mutable.Set[ListBuffer[Instruction]] = mutable.Set()

  /* Used to store the string literals required by the program */
  private val stringPool: mutable.Set[AscizInstr] = mutable.Set()

  /* Generates a unique label for a String literal */
  private def getStringLabel: String = {
    stringCounter += 1
    ".L._str" + stringCounter
  }

  /* Generates a unique label */
  private def getUniqueLabel: String = {
    labelCounter += 1
    ".L" + labelCounter
  }

  /* Generates instructions for an ASTNode */
  def generateInstructions(astNode: ASTNode, allocator: BasicRegisterAllocator, dest: Register): ListBuffer[Instruction] = {

    /* Generates instructions for a program */
    def programGenerate(funcs: List[Function], stmts: Statement): ListBuffer[Instruction] = {
      val programLines = ListBuffer[Instruction]()

      /* Generates instructions for the main body of the program */
      val stmtLines = generateInstructions(stmts, allocator, dest)

      /* Generates instructions for the functions */
      val funcsLines = ListBuffer[Instruction]()

      for (func <- funcs) {
        /* Assign a new register allocator for each function */
        val funcAllocator = new BasicRegisterAllocator
        val (funcReg, _) = funcAllocator.allocateRegister()
        val funcLines = generateInstructions(func, funcAllocator, funcReg)
        funcsLines ++= funcLines
      }

      programLines ++= List(Comment("Start of program", 4),
        Command("data", 0))
      programLines ++= stringPool.toList
      programLines ++= List(Command("align 4", 0),
        Command("text", 0), Command("global main", 0),
        Label("main"),
        Push(List(FP, LR)),
        Mov(FP, SP))
      programLines ++= stmtLines
      programLines ++= List(Mov(R0, ImmVal(0)),
        Pop(List(FP, PC)))
      programLines ++= funcsLines
      programLines ++= predefinedFunctions.foldLeft(List[Instruction]())(_ ++ _)

      programLines
    }

    /* Generates instructions for a function */
    def functionGenerate(_type: Type, funcName: Ident, params: List[Param], body: Statement): ListBuffer[Instruction] = {
      val funcLines = ListBuffer[Instruction]()
      allocator.setLocation(funcName.nickname.get, VariableLocation(R0, 0, 4, _type))

      funcLines ++= List(Comment("Start of function", 4),
        Label(funcName.nickname.get),
        Push(List(FP, LR)),
        Push(List(R4, R5, R6, R7, R8, R9, R10)),
        Mov(FP, SP))
      /* Generates instructions for the function parameters */
      funcLines ++= paramsGenerate(params)
      /* Generate instructions for function body */
      body match {
        case Statements(stmts) =>
          stmts.foreach { stmt =>
            funcLines += Push(List(R0, R1, R2, R3))
            funcLines ++= generateInstructions(stmt, allocator, dest)
            funcLines += Pop(List(R0, R1, R2, R3))
          }
        case _ =>
          funcLines += Push(List(R0, R1, R2, R3))
          funcLines ++= generateInstructions(body, allocator, dest)
          funcLines += Pop(List(R0, R1, R2, R3))
      }
      funcLines += Command("ltorg", 4)

      funcLines
    }

    /* Generates instructions to retrieve a list of parameters */
    def paramsGenerate(params: List[Param]): ListBuffer[Instruction] = {
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
            paramLines ++= rLines
            paramLines += StrInstr(next, Addr(FP, ImmVal(4 * (params.length - i - 1))))
            allocator.setLocation(param.ident.nickname.get, VariableLocation(next, 0, 4, param._type))
        }
      }
      paramLines
    }

    /* Generates instructions for a declare statement */
    def declareGenerate(_type: Type, id: Ident, value: RValue): ListBuffer[Instruction] = {
      val declareLines = ListBuffer[Instruction]()

      /* Allocate the register newDest for the identifier */
      val (newDest, rLines) = allocator.allocateRegister()
      allocator.setLocation(id.nickname.get, VariableLocation(newDest, 0, 4, _type))
      val idLines = generateInstructions(id, allocator, newDest)

      /* Generate instructions to store the rvalue in newDest */
      val valueLines = generateInstructions(value, allocator, newDest)

      declareLines += Comment("Start of declare", 4)
      declareLines ++= rLines
      declareLines ++= idLines
      declareLines ++= valueLines

      declareLines
    }

    /* Generates instructions for an assign statement */
    def assignGenerate(lvalue: LValue, rvalue: RValue): ListBuffer[Instruction] = {
      val assignLines = ListBuffer[Instruction]()

      /* Generate instructions to load rvalue into dest register */
      val rvalueLines = generateInstructions(rvalue, allocator, dest)

      /* Get the target register which contains the lvalue */
      val (beforeLines, afterLines, target) = getLvalueLocation(lvalue)
      /* Store the rvalue in the target register */

      assignLines += Comment("Start of assign", 4)
      assignLines ++= rvalueLines
      assignLines ++= beforeLines
      assignLines += Mov(target, dest)
      assignLines ++= afterLines
      assignLines += Comment("End of assign", 4)

      assignLines
    }

    /* Generates instructions to retrieve and store the lvalue,
           as well as returning the target register that must be assigned to */
    def getLvalueLocation(lvalue: LValue): (ListBuffer[Instruction], ListBuffer[Instruction], Register) = {
      lvalue match {
        /* Returns register location of identifier */
        case Ident(_, nickname, _) =>
          val identLoc: VariableLocation = allocator.lookupLocation(nickname.get).get
          (ListBuffer(), ListBuffer(), identLoc.register)
        /* Returns register location of array element as well as instructions for loading and storing to the array */
        case ArrayElem(ident, indices) =>
          val identLoc: VariableLocation = allocator.lookupLocation(ident.nickname.get).get
          val (before, after, target) = getArrayElemLocation(identLoc._type, identLoc.register, indices)
          (before, after, target)
        /* Returns register location of pair element as well as instructions for loading and storing to the pair */
        case PairElem(func, lvalue) =>
          predefinedFunctions += errorNullFunc
          val (newDest, rLines) = allocator.allocateRegister()
          func match {
            case "fst" =>
              val fstLines1 = ListBuffer[Instruction]()
              val fstLines2 = ListBuffer[Instruction]()
              val (beforeLines, afterLines, lvalueLoc) = getLvalueLocation(lvalue)
              allocator.deallocateRegister(newDest)

              fstLines1 ++= beforeLines
              fstLines1 ++= pairLines(lvalue, lvalueLoc)
              fstLines1 ++= rLines
              fstLines1 += Ldr(newDest, Addr(lvalueLoc, ImmVal(0)))

              fstLines2 ++= pairLines(lvalue, lvalueLoc)
              fstLines2 += StrInstr(newDest, Addr(lvalueLoc, ImmVal(0)))
              fstLines2 ++= afterLines

              (fstLines1, fstLines2, newDest)
            case "snd" =>
              val sndLines1 = ListBuffer[Instruction]()
              val sndLines2 = ListBuffer[Instruction]()
              val (beforeLines, afterLines, lvalueLoc) = getLvalueLocation(lvalue)
              allocator.deallocateRegister(newDest)

              sndLines1 ++= pairLines(lvalue, lvalueLoc)
              sndLines1 ++= rLines
              sndLines1 += Ldr(newDest, Addr(lvalueLoc, ImmVal(4)))
              sndLines1 ++= beforeLines

              sndLines2 ++= afterLines
              sndLines2 ++= pairLines(lvalue, lvalueLoc)
              sndLines2 += StrInstr(newDest, Addr(lvalueLoc, ImmVal(0)))

              (sndLines1, sndLines2, newDest)
          }
      }
    }

    /* Checks that a pair pointer is not null */
    def pairLines(lvalue: LValue, loc: Register): ListBuffer[Instruction] = {
      lvalue.getType match {
        case PairT(_, _) => ListBuffer(
          CmpInstr(loc, ImmVal(0)),
          BInstr("_errNull", EQcond),
        )
        case _ => ListBuffer()
      }
    }

    /* Checks if the register is R0, R3 or R8 and allocates a new register if it is */
    def checkRegister(regInstr: (Register, ListBuffer[Instruction])): (Register, ListBuffer[Instruction]) = {
      val (register, instr) = regInstr
      if (register == R0 || register == R3 || register == R8) {
        allocator.allocateRegister()
      }
      (register, instr)
    }

    /* Recursively returns the register location of array element as well as instructions for loading and storing to the array */
    def getArrayElemLocation(arrayType: Type, arrayReg: Register, indices: List[Expr]): (ListBuffer[Instruction], ListBuffer[Instruction], Register) = {
      predefinedFunctions += arrayLoad4Func
      if (indices.isEmpty) {
        return (ListBuffer(), ListBuffer(), arrayReg)
      }
      val beforeLines = new ListBuffer[Instruction]()
      val afterLines = new ListBuffer[Instruction]()
      val (indexReg, r1Lines) = checkRegister(allocator.allocateRegister())
      val (elemReg, r2Lines) = checkRegister(allocator.allocateRegister())

      val indexLines = generateInstructions(indices.head, allocator, indexReg)
      beforeLines ++= r1Lines
      beforeLines ++= r2Lines
      beforeLines ++= indexLines
      beforeLines ++= List(
        Push(List(R0, R1)),
        Mov(R0, indexReg),
        Mov(R3, arrayReg),
        BInstr("_arrLoad4", noCondition, storeReturnAddr = true),
        Pop(List(R0, R1)),
        Mov(elemReg, R3)
      )
      val (before, after, target) = getArrayElemLocation(reduceType(arrayType), elemReg, indices.tail)
      beforeLines ++= before
      val storeFunc = getTypeSize(reduceType(arrayType)) match {
        case 1 =>
          predefinedFunctions += arrayStore1Func
          "_arrStore1"
        case _ =>
          predefinedFunctions += arrayStore4Func
          "_arrStore4"
      }
      afterLines ++= after
      afterLines ++= List(
        Push(List(R0, R1, R8)),
        Mov(R0, indexReg),
        Mov(R3, arrayReg),
        Mov(R8, elemReg),
        BInstr(storeFunc, noCondition, storeReturnAddr = true),
        Pop(List(R0, R1, R8)),
        Mov(arrayReg, R3)
      )
      allocator.deallocateRegister(indexReg)
      allocator.deallocateRegister(elemReg)
      (beforeLines, afterLines, target)
    }

    /* Reduces the dimensions of an array by 1 */
    def reduceType(t: Type): Type = {
      t match {
        case ArrayT(tp, d) if d > 1 => ArrayT(tp, d - 1)
        case ArrayT(tp, 1) => tp
        case _ => t
      }
    }

    /* Generates instructions for a read statement */
    def readGenerate(lvalue: LValue): ListBuffer[Instruction] = {
      val readLines = ListBuffer[Instruction]()
      val _type = lvalue.getType match {
        case BaseT("int") =>
          predefinedFunctions += readIntFunc
          "i"
        case BaseT("char") =>
          predefinedFunctions += readCharFunc
          "c"
        case _ => ""
      }
      val (beforeLines, afterLines, target) = getLvalueLocation(lvalue)

      readLines += Comment("Start of read", 4)
      readLines ++= beforeLines
      readLines ++= List(Mov(dest, target),
        Mov(R0, dest),
        BInstr(s"_read${_type}", noCondition, storeReturnAddr = true),
        Mov(target, R0))
      readLines ++= afterLines

      readLines
    }

    /* Generates instructions for an if statement */
    def ifGenerate(cond: Expr, thenS: Statement, elseS: Statement): ListBuffer[Instruction] = {
      val ifLines = ListBuffer[Instruction]()
      val elseLabel = getUniqueLabel
      val endLabel = getUniqueLabel
      val condLines = generateInstructions(cond, allocator, dest)
      val thenLines = generateInstructions(thenS, allocator, dest)
      val elseLines = generateInstructions(elseS, allocator, dest)

      ifLines += Comment("Start of if statement", 4)
      ifLines ++= condLines
      ifLines ++= List(Comment("If statement condition logic", 4),
        CmpInstr(dest, ImmVal(1)),
        BInstr(elseLabel, NEcond))
      ifLines ++= thenLines
      ifLines ++= List(BInstr(endLabel),
        Label(elseLabel))
      ifLines ++= elseLines
      ifLines ++= List(
        Label(endLabel),
        Comment("End of if statement", 4))

      ifLines
    }

    /* Generates instructions for a while statement */
    def whileGenerate(cond: Expr, stmt: Statement): ListBuffer[Instruction] = {
      val whileLines = ListBuffer[Instruction]()
      val startLabel = getUniqueLabel
      val endLabel = getUniqueLabel
      val condLines = generateInstructions(cond, allocator, dest)
      val (newDest, rLines) = allocator.allocateRegister()
      val stmtLines = generateInstructions(stmt, allocator, newDest)

      whileLines ++= List(Comment("Start of while loop", 4),
        Label(startLabel))
      whileLines ++= condLines
      whileLines ++= List(Comment("While loop condition logic", 4),
        CmpInstr(dest, ImmVal(1)),
        BInstr(endLabel, NEcond))
      whileLines ++= rLines
      whileLines ++= stmtLines
      whileLines ++= List(BInstr(startLabel),
        Label(endLabel),
        Comment("End of while loop", 4))

      whileLines
    }

    /* Generates instructions for a new scope */
    def scopeGenerate(body: Statement): ListBuffer[Instruction] = {
      val scopeLines = ListBuffer[Instruction]()
      val bodyLines = generateInstructions(body, allocator, dest)

      scopeLines += Comment("Start of new scope", 4)
      scopeLines ++= bodyLines
      scopeLines += Comment("End of new scope", 4)

      scopeLines
    }

    /* Generates instructions for a free statement */
    def freeGenerate(exp: Expr): ListBuffer[Instruction] = {
      val freeLines = ListBuffer[Instruction]()
      predefinedFunctions += freeFunc
      val expLines = generateInstructions(exp, allocator, dest)
      val (_type, tLines) = exp.getType match {
        case PairT(_, _) =>
          predefinedFunctions += freePairFunc
          ("pair", List())
        case ArrayT(_, _) =>
          ("", List(SubInstr(dest, dest, ImmVal(4))))
        case _ => ("", List())
      }

      freeLines += Comment("Start of free", 4)
      freeLines ++= expLines
      freeLines ++= tLines
      freeLines ++= List(Mov(R0, dest),
        BInstr("_free" + _type, noCondition, storeReturnAddr = true))

      freeLines
    }

    /* Generates instructions for a return statement */
    def returnGenerate(exp: Expr): ListBuffer[Instruction] = {
      val returnLines = ListBuffer[Instruction]()
      val expLines = generateInstructions(exp, allocator, dest)

      returnLines += Comment("Start of return", 4)
      returnLines ++= expLines
      returnLines ++= List(Comment("Return Logic", 4),
        Mov(R0, dest),
        Mov(SP, FP),
        Pop(List(R4, R5, R6, R7, R8, R9, R10)),
        Pop(List(FP, PC)))

      returnLines
    }

    /* Generates instructions for an exit statement */
    def exitGenerate(exp: Expr): ListBuffer[Instruction] = {
      predefinedFunctions += exitFunc
      val exitLines = ListBuffer[Instruction]()
      val expLines = generateInstructions(exp, allocator, dest)

      exitLines += Comment("Start of exit", 4)
      exitLines ++= expLines
      exitLines ++= List(Comment("Exit Logic", 4),
        Mov(R0, dest),
        BInstr("_exit", noCondition, storeReturnAddr = true),
        Mov(SP, FP),
        Pop(List(FP, PC)))

      exitLines
    }

    /* Generates instructions for a print statement */
    def printGenerate(exp: Expr): ListBuffer[Instruction] = {
      var _type: Type = exp.getType
      // Get the type of the Ident stored in the node
      exp match {
        case Ident(_, _, t) =>
          t match {
            case Some(x) => _type = x
            case None => BaseT("ERROR")
          }
        case ArrayElem(ident, indices) =>
          // Get the type of the array element from type stored in the Ident
          ident._type match {
            case Some(t) =>
              // Reduce the type of the ArrayT
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
          predefinedFunctions += printCharOrIntFunc(false)
          "i"
        case BaseT("char") =>
          predefinedFunctions += printCharOrIntFunc(true)
          "c"
        case PairLiter(_) =>
          predefinedFunctions += printPairFunc
          "p"
        case BaseT("string") =>
          predefinedFunctions += printStrFunc
          "s"
        case ArrayT(BaseT("char"), 1) =>
          predefinedFunctions += printStrFunc
          "s"
        case BaseT("bool") =>
          predefinedFunctions += printBoolFunc
          "b"
        case PairT(_, _) =>
          predefinedFunctions += printPairFunc
          "p"
        case ArrayT(_, _) =>
          predefinedFunctions += printPairFunc
          "p"
        case _ => "error"
      }

      val printLines = ListBuffer[Instruction]()
      val expLines = generateInstructions(exp, allocator, dest)

      printLines += Comment("Start of print", 4)
      printLines ++= expLines
      printLines ++= List(Comment("Print Logic", 4),
        Mov(R0, dest),
        BInstr(s"_print$t", noCondition, storeReturnAddr = true))

      printLines
    }

    /* Generates instructions for a println statement */
    def printlnGenerate(exp: Expr): ListBuffer[Instruction] = {
      predefinedFunctions += printLnFunc
      val printlnLines = ListBuffer[Instruction]()

      printlnLines ++= printGenerate(exp)
      printlnLines += BInstr("_println", noCondition, storeReturnAddr = true)

      printlnLines
    }

    /* Generates instructions to create an array literal */
    def arrayLiterGenerate(elems: List[Expr]): ListBuffer[Instruction] = {
      predefinedFunctions += mallocFunc
      val arrayLiterLines = ListBuffer[Instruction]()
      val (pointer, r1Lines) = allocator.allocateRegister()
      val arrayLines = new ListBuffer[Instruction]()
      var totalSize = 0
      for (elem <- elems) {
        val (next, r2Lines) = allocator.allocateRegister()
        val elemLines = generateInstructions(elem, allocator, next)
        val size = getSize(elem)
        val sizeToStore = size match {
          case 1 => OneByte
          case _ => FourBytes
        }
        totalSize += size
        arrayLines ++= r2Lines
        arrayLines ++= elemLines
        arrayLines ++= List(Mov(dest, next),
          StrInstr(dest, Addr(pointer, ImmVal(totalSize - size)), sizeToStore))

        allocator.deallocateRegister(next)
      }
      totalSize += 4
      allocator.deallocateRegister(pointer)

      arrayLiterLines += Comment("Start of array literal", 4)
      arrayLiterLines ++= r1Lines
      arrayLiterLines ++= List(Mov(R0, ImmVal(totalSize)),
        BInstr("_malloc", noCondition, storeReturnAddr = true),
        Mov(pointer, R0),
        AddInstr(pointer, pointer, ImmVal(4)),
        Mov(dest, ImmVal(elems.length)),
        StrInstr(dest, Addr(pointer, ImmVal(-4))))
      arrayLiterLines ++= arrayLines
      arrayLiterLines += Mov(dest, pointer)

      arrayLiterLines
    }

    /* Returns the size of expression in bytes */
    def getSize(expr: Expr): Int = {
      getTypeSize(expr.getType)
    }

    /* Returns the size of a type in bytes */
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

    /* Generates instructions to create a new pair */
    def newPairGenerate(exp1: Expr, exp2: Expr): ListBuffer[Instruction] = {
      predefinedFunctions += mallocFunc
      val newPairLines = ListBuffer[Instruction]()
      val (next, rLines) = allocator.allocateRegister()

      newPairLines += Comment("Start of new pair creation", 4)
      newPairLines ++= rLines
      newPairLines ++= List(
        Mov(R0, ImmVal(8)),
        BInstr("_malloc", noCondition, storeReturnAddr = true),
        Mov(dest, R0),
      )
      newPairLines ++= generateInstructions(exp1, allocator, next)
      newPairLines += StrInstr(next, Addr(dest, ImmVal(0)))
      newPairLines ++= generateInstructions(exp2, allocator, next)
      newPairLines ++= List(
        StrInstr(next, Addr(dest, ImmVal(4))),
        Comment("End of new pair creation", 4)
      )
      allocator.deallocateRegister(next)
      newPairLines
    }

    /* Generates instructions for an expression */
    def getExpLines (exp: Either[LValue, Expr]): (ListBuffer[Instruction], ListBuffer[Instruction], Register) = {
      exp.fold(
        x => generateExpLines(x),
        x => generateExpLines(x)
      )
    }

    /* Generates instructions for an expression */
    def generateExpLines(x: ASTNode): (ListBuffer[Instruction], ListBuffer[Instruction], Register) = {
      val (next, rLines) = allocator.allocateRegister()
      val expLines = generateInstructions(x, allocator, next)
      allocator.deallocateRegister(next)
      (expLines, rLines, next)
    }

    /* Generates instructions for extracting a pair element */
    def pairElemGenerate(func: String, lvalue: LValue): ListBuffer[Instruction] = {
      predefinedFunctions += errorNullFunc
      val pairElemLines = ListBuffer[Instruction]()
      val (lvalueLines, rLines, next) = getExpLines(Left(lvalue))
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

      pairElemLines += Comment("Start of pair element", 4)
      pairElemLines ++= rLines
      pairElemLines ++= lvalueLines
      pairElemLines ++= List(
        CmpInstr(next, ImmVal(0)),
        BInstr("_errNull", EQcond)
      )
      pairElemLines ++= funcLines

      pairElemLines
    }

    /* Generates instructions to evaluate two expressions */
    def addMulGenerate(exp1: Expr, exp2: Expr): (ListBuffer[Instruction], ListBuffer[Instruction]) = {
      predefinedFunctions += errorOverflowFunc
      val exp1Lines = generateInstructions(exp1, allocator, dest)
      val exp2Lines = generateInstructions(exp2, allocator, dest)
      (exp1Lines, exp2Lines)
    }

    /* Generates instructions for a multiplication expression */
    def mulGenerate(exp1: Expr, exp2: Expr): ListBuffer[Instruction] = {
      val mulLines = ListBuffer[Instruction]()
      val (exp1Lines, exp2Lines) = addMulGenerate(exp1, exp2)
      val (val1, r1Lines) = allocator.allocateRegister()
      val (val2, r2Lines) = allocator.allocateRegister()
      val (hi, r3Lines) = allocator.allocateRegister()
      allocator.deallocateRegister(hi)
      allocator.deallocateRegister(val1)
      allocator.deallocateRegister(val2)

      mulLines += Comment("Start of multiplication", 4)
      mulLines ++= exp1Lines
      mulLines += Push(List(dest))
      mulLines ++= exp2Lines
      mulLines += Push(List(dest))
      mulLines ++= r1Lines
      mulLines ++= r2Lines
      mulLines ++= r3Lines
      mulLines ++= List(
        Pop(List(val1, val2).sortBy(_.number)),
        SmullInstr(dest, hi, val1, val2),
        CmpInstr(hi, RegShift(dest, ShiftRight(31))),
        BInstr("_errOverflow", NEcond, storeReturnAddr = true),
        Comment("End of multiplication expression", 4)
      )

      mulLines
    }

    /* Generates instructions for a division or modulo expression */
    def divModGenerate(exp1: Expr, exp2: Expr): ListBuffer[Instruction] = {
      predefinedFunctions += errorDivByZeroFunc
      val divModLines = ListBuffer[Instruction]()
      val exp1Lines = generateInstructions(exp1, allocator, dest)
      val exp2Lines = generateInstructions(exp2, allocator, dest)

      divModLines ++= exp1Lines
      divModLines += Mov(R0, dest)
      divModLines ++= exp2Lines
      divModLines ++= List(
        Mov(R1, dest),
        CmpInstr(R1, ImmVal(0)),
        BInstr("_errDivZero", EQcond, storeReturnAddr = true),
        BInstr("__aeabi_idivmod", noCondition, storeReturnAddr = true),
        Mov(dest, dest)
      )

      divModLines
    }

    /* Generates instructions for a division expression */
    def divGenerate(exp1: Expr, exp2: Expr): ListBuffer[Instruction] = {
      val divLines = divModGenerate(exp1, exp2)

      divLines += Comment("Start of division", 4)
      divLines ++= divModGenerate(exp1, exp2)
      divLines += Mov(dest, R0)

      divLines
    }

    /* Generates instructions for a modulo expression */
    def modGenerate(exp1: Expr, exp2: Expr): ListBuffer[Instruction] = {
      val modLines = divModGenerate(exp1, exp2)

      modLines += Comment("Start of modulo", 4)
      modLines ++= divModGenerate(exp1, exp2)
      modLines += Mov(dest, R0)

      modLines
    }

    /* Generates instructions for an addition expression */
    def addGenerate(exp1: Expr, exp2: Expr): ListBuffer[Instruction] = {
      val addLines = ListBuffer[Instruction]()
      val (exp1Lines, exp2Lines) = addMulGenerate(exp1, exp2)
      val (next, rLines) = allocator.allocateRegister()
      allocator.deallocateRegister(next)

      addLines += Comment("Start of addition expression", 4)
      addLines ++= exp2Lines
      addLines += Push(List(dest))
      addLines ++= exp1Lines
      addLines ++= rLines
      addLines ++= List(
        Pop(List(next)),
        AddInstr(dest, dest, next, updateFlags = true),
        BInstr("_errOverflow", VScond, storeReturnAddr = true),
        Comment("End of addition expression", 4)
      )

      addLines
    }

    /* Generates instructions to evaluate exp1 into dest and exp2 into next */
    def exprsGenerateHelper(exp1: Expr, exp2: Expr): (ListBuffer[Instruction], ListBuffer[Instruction], ListBuffer[Instruction], Register) = {
      val exp1Lines = generateInstructions(exp1, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      val exp2Lines = generateInstructions(exp2, allocator, next)
      allocator.deallocateRegister(next)
      (exp1Lines, rLines, exp2Lines, next)
    }

    /* Generates instructions for a subtraction expression */
    def subGenerate(exp1: Expr, exp2: Expr): ListBuffer[Instruction] = {
      predefinedFunctions += errorOverflowFunc
      val subLines = ListBuffer[Instruction]()
      val (exp1Lines, rLines, exp2Lines, next) = exprsGenerateHelper(exp1, exp2)

      subLines += Comment("Start of subtraction expression", 4)
      subLines ++= exp1Lines
      subLines ++= rLines
      subLines ++=  exp2Lines
      subLines ++= List(
        SubInstr(dest, dest, next, updateFlags = true),
        BInstr("_errOverflow", VScond, storeReturnAddr = true),
        Comment("End of subtraction expression", 4)
      )

      subLines
    }

    /* Generates instructions for a conditional expression */
    def condGenerate(exp1: Expr, exp2: Expr, cond: Condition): ListBuffer[Instruction] = {
      val condLines = ListBuffer[Instruction]()
      val (exp1Lines, rLines, exp2Lines, next) = exprsGenerateHelper(exp1, exp2)

      condLines ++= exp1Lines
      condLines ++= rLines
      condLines ++= exp2Lines
      condLines ++= List(
        CmpInstr(dest, next),
        Mov(dest, ImmVal(0)),
        Mov(dest, ImmVal(1), cond)
      )

      condLines
    }

    /* Generates instructions for an and expression */
    def andGenerate(exp1: Expr, exp2: Expr): ListBuffer[Instruction] = {
      val andLines = ListBuffer[Instruction]()
      val (exp1Lines, rLines, exp2Lines, next) = exprsGenerateHelper(exp1, exp2)

      andLines += Comment("Start of and expression", 4)
      andLines ++= exp1Lines
      andLines ++= rLines
      andLines ++= exp2Lines
      andLines ++= List(
        CmpInstr(next, ImmVal(0)),
        Mov(dest, ImmVal(0), EQcond),
        Comment("End of and expression", 4)
      )

      andLines
    }

    /* Generates instructions for an or expression */
    def orGenerate(exp1: Expr, exp2: Expr): ListBuffer[Instruction] = {
      val orLines = ListBuffer[Instruction]()
      val (exp1Lines, rLines, exp2Lines, next) = exprsGenerateHelper(exp1, exp2)

      orLines += Comment("Start of or expression", 4)
      orLines ++= exp1Lines
      orLines ++= rLines
      orLines ++= exp2Lines
      orLines ++= List(
        CmpInstr(next, ImmVal(1)),
        Mov(dest, ImmVal(1), EQcond),
        Comment("End of or expression", 4)
      )

      orLines
    }

    /* Generates instructions for a not expression */
    def notGenerate(exp: Expr): ListBuffer[Instruction] = {
      val notLines = ListBuffer[Instruction]()
      val expLines = generateInstructions(exp, allocator, dest)

      notLines += Comment("Start of not expression", 4)
      notLines ++= expLines
      notLines ++= List(
        CmpInstr(dest, ImmVal(0)),
        Mov(dest, ImmVal(0), NEcond),
        Mov(dest, ImmVal(1), EQcond),
        Comment("End of not expression", 4)
      )

      notLines
    }

    /* Generates instructions for a negation expression */
    def negGenerate(exp: Expr): ListBuffer[Instruction] = {
      predefinedFunctions += errorOverflowFunc
      val negLines = ListBuffer[Instruction]()
      val (expLines, rLines, tempReg) = getExpLines(Right(exp))

      negLines += Comment("Start of negation expression", 4)
      negLines ++= rLines
      negLines ++= expLines
      negLines ++= List(
        RsbsInstr(dest, tempReg, ImmVal(0)),
        BInstr("_errOverflow", VScond, storeReturnAddr = true),
        Comment("End of negation expression", 4)
      )

      negLines
    }

    /* Generates instructions for a length expression */
    def lenGenerate(exp: Expr): ListBuffer[Instruction] = {
      val lenLines = ListBuffer[Instruction]()
      val (next, rLines) = allocator.allocateRegister()
      val expLines = generateInstructions(exp, allocator, next)
      allocator.deallocateRegister(next)
      lenLines += Comment("Start of length expression", 4)
      lenLines ++= rLines
      lenLines ++= expLines
      lenLines ++= List(
        Ldr(dest, Addr(next, ImmVal(-4))),
        Comment("End of length expression", 4)
      )

      lenLines
    }

    /* Generates instructions for an ord expression */
    def ordGenerate(exp: Expr): ListBuffer[Instruction] = {
      val ordLines = ListBuffer[Instruction]()
      val expLines = generateInstructions(exp, allocator, dest)

      ordLines += Comment("Start of ord", 4)
      ordLines ++= expLines
      ordLines += Comment("End of ord", 4)

      ordLines
    }

    /* Generates instructions for a chr expression */
    def chrGenerate(exp: Expr): ListBuffer[Instruction] = {
      predefinedFunctions += errorBadCharFunc
      val chrLines = ListBuffer[Instruction]()
      val (next, rLines) = allocator.allocateRegister()
      val expLines = generateInstructions(exp, allocator, dest)

      chrLines += Comment("Start of chr", 4)
      chrLines ++= expLines
      chrLines ++= rLines
      chrLines ++= List(
        Mov(next, ImmVal(-128)),
        Tst(dest, next),
        Mov(R1, dest, NEcond),
        BInstr("_errBadChar", NEcond, storeReturnAddr = true),
        Comment("End of chr expression", 4)
      )

      chrLines
    }

    /* Generates instructions for loading a number into dest */
    def numGenerate(n: Int): ListBuffer[Instruction] = {
      val numLines = ListBuffer[Instruction]()

      numLines += Comment("Start of number", 4)

      if (n < 0 || n > 255) {
        numLines += Ldr(dest, IntLiteral(n))
      } else {
        numLines += Mov(dest, ImmVal(n))
      }
      numLines
    }

    /* Generates instructions for loading a boolean into dest */
    def boolGenerate(b: String): ListBuffer[Instruction] = {
      val boolLines = ListBuffer[Instruction]()
      boolLines += Comment("Start of boolean", 4)
      b match {
        case "true" =>
          boolLines += Mov(dest, ImmVal(1))
        case "false" =>
          boolLines += Mov(dest, ImmVal(0))
      }
      boolLines
    }

    /* Generates instructions for loading a character into dest */
    def chGenerate(c: Char): ListBuffer[Instruction] = {
      val chLines = ListBuffer[Instruction]()

      chLines ++= List(
        Comment("Start of character", 4),
        Mov(dest, ImmVal(c.toInt))
      )

      chLines
    }

      /* Generates instructions for loading an identifier into dest */
    def identGenerate(n: String): ListBuffer[Instruction] = {
      val identLines = ListBuffer[Instruction]()
      val location: Option[VariableLocation] = allocator.lookupLocation(n)

      identLines += Comment("Start of identifier ", 4)
      identLines ++= (location match {
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
      })

      identLines += Comment("End of identifier", 4)

      identLines
    }

    /* Generates instructions for loading a string into dest */
    def strGenerate(s: String): ListBuffer[Instruction] = {
      val strLines = ListBuffer[Instruction]()
      val label = getStringLabel
      val asciz = AscizInstr(label, StringLiteral(s))
      stringPool += asciz

      strLines ++= List(
        Comment("Start of string", 4),
        Ldr(dest, LabelAddr(label))
      )

      strLines
    }

    /* Generates instructions for loading an array element into dest */
    def arrayElemGenerate(id: Ident, indices: List[Expr]): ListBuffer[Instruction] = {
      predefinedFunctions += arrayLoad4Func
      val arrayElemLines = ListBuffer[Instruction]()
      val idLines = generateInstructions(id, allocator, dest)
      val indicesLines = new ListBuffer[Instruction]()
      for (index <- indices) {
        /* Check if allocated register is R10, R3 or R8 and allocate a new register if it is */
        val (next, rLines) = checkRegister(allocator.allocateRegister())
        val indexLines = generateInstructions(index, allocator, next)
        indicesLines ++= rLines
        indicesLines ++= indexLines
        indicesLines ++= List(
          Mov(R0, next),
          Mov(R3, dest),
          BInstr("_arrLoad4", noCondition, storeReturnAddr = true),
          Mov(dest, R3)
        )
        allocator.deallocateRegister(next)
      }

      arrayElemLines += Comment("Start of array element", 4)
      arrayElemLines ++= idLines
      arrayElemLines ++= indicesLines

      arrayElemLines
    }

    /* Generates instructions for calling a function */
    def callGenerate(funcName: Ident, args: List[Expr]): ListBuffer[Instruction] = {
      val callLines = ListBuffer[Instruction] ()

      callLines ++= List (
        Comment ("Start of function call", 4),
        Push (List (R1, R2, R3) ) )
      callLines ++= argsGenerate (args)
      callLines ++= List (
        BInstr (funcName.nickname.get, noCondition, storeReturnAddr = true),
        Pop (List (R1, R2, R3) ),
        Mov (dest, R0),
        Comment ("End of function call", 4)
      )
      callLines
    }

    /* Generates instructions for function call arguments */
    def argsGenerate(args: List[Expr]): ListBuffer[Instruction] = {
      val argsLines = new ListBuffer[Instruction]()
      for (i <- args.indices) {
        val arg = args(i)
        val (next, rLines) = allocator.allocateRegister()
        val argLines = generateInstructions(arg, allocator, next)
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
      argsLines
    }

    /* Identifies the ast node and generates the corresponding instructions */
    astNode match {
      case Program(funcs, stmts) =>
        programGenerate(funcs, stmts)

      case Function(_type, funcName, paramList, body) =>
        functionGenerate(_type, funcName, paramList, body)

      case Skip() =>
        ListBuffer(Comment("Skip", 4))

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
        val stmtLines = ListBuffer() ++= stmts.flatMap(generateInstructions(_, allocator, dest))
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
        condGenerate(x.exp1, x.exp2, x.getCond())
      
      case x: Equality =>
        condGenerate(x.exp1, x.exp2, x.getCond())

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
        ListBuffer(
          Comment("Start of pair literal", 4),
          Mov(dest, ImmVal(0))
        )

      case Ident(_, n, _) =>
        n match {
          case Some(v) => identGenerate(v)
          case None => ListBuffer()
        }

      case ArrayElem(f, p) =>
        arrayElemGenerate(f, p)

      case _ => ListBuffer[Instruction]()
    }
  }
}
