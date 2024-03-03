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
  val predefinedFunctions: mutable.Set[List[Instruction]] = mutable.Set()

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
  def generateInstructions(astNode: ASTNode, allocator: BasicRegisterAllocator, dest: Register): List[Instruction] = {

    /* Generates instructions for a program */
    def programGenerate(funcs: List[Function], stmts: Statement): List[Instruction] = {
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
      List(
        Comment("Start of program", 0),
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
      funcsLines ++
      predefinedFunctions.foldLeft(List[Instruction]())(_ ++ _)
    }

   /* Generates instructions for a function */
    def functionGenerate(_type: Type, funcName: Ident, params: List[Param], body: Statement): List[Instruction] = {
      allocator.setLocation(funcName.nickname.get, VariableLocation(R0, 0, 4, _type))
      val stmts = body match {
        case Statements(stmts) => stmts
        case _ => List(body)
      }
      Comment("Start of function", 0) ::
      Label(funcName.nickname.get) ::
      List(
        Push(List(FP, LR)),
        Push(List(R4, R5, R6, R7, R8, R9, R10)),
        Mov(FP, SP),
        SubInstr(SP, SP, ImmVal(4 * params.length - 4))
      ) ++
      /* Generates instructions for the function parameters */
      paramsGenerate(params) ++
      /* Generate instructions for function body */
      stmts.flatMap(
        Push(List(R0, R1, R2, R3)) :: 
        generateInstructions(_, allocator, dest) ++ 
        List(Pop(List(R0, R1, R2, R3)))
      ) ++
      List(
        Command("ltorg", 4)
      )
    }

    /* Generates instructions to retrieve a list of parameters */
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
            paramLines ++= rLines
            paramLines += Ldr(next, Addr(FP, ImmVal(4 * (params.length - i - 1))))
            allocator.setLocation(param.ident.nickname.get, VariableLocation(next, 0, 4, param._type))
        }
      }
      paramLines.toList
    }

    /* Generates instructions for a declare statement */
    def declareGenerate(_type: Type, id: Ident, rvalue: RValue): List[Instruction] = {
      /* Allocate the register newDest for the identifier */
      val (newDest, rLines) = allocator.allocateRegister()
      allocator.setLocation(id.nickname.get, VariableLocation(newDest, 0, 4, _type))
      val idLines = generateInstructions(id, allocator, newDest)
      /* Generate instructions to store the rvalue in newDest */
      val rvalueLines = generateInstructions(rvalue, allocator, newDest)
      Comment("Start of declare statement", 4) ::
      rLines ++
      idLines ++
      rvalueLines ++
      List(Comment("End of declare statement", 4))
    }

    /* Generates instructions for an assign statement */
    def assignGenerate(lvalue: LValue, rvalue: RValue): List[Instruction] = {
      /* Generate instructions to load rvalue into dest register */
      val rvalueLines = generateInstructions(rvalue, allocator, dest)
      /* Get the target register which contains the lvalue */
      val (beforeLines, afterLines, target) = getLvalueLocation(lvalue)
      /* Store the rvalue in the target register */
      Comment("Start of assign statement", 4) ::
      rvalueLines ++
      beforeLines ++
      List(Mov(target, dest)) ++
      afterLines ++
      List(Comment("End of assign statement", 4))
    }

    /* Generates instructions to retrieve and store the lvalue, 
       as well as returning the target register that must be assigned to */
    def getLvalueLocation(lvalue: LValue): (List[Instruction], List[Instruction], Register) = {
      lvalue match {
        /* Returns register location of identifier */
        case Ident(_, nickname, _) =>
          val identLoc: VariableLocation = allocator.lookupLocation(nickname.get).get
          (List(), List(), identLoc.register)
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

    /* Checks that a pair pointer is not null */
    def pairLines(lvalue: LValue, loc: Register): List[Instruction] = {
      lvalue.getType match {
        case PairT(_, _) => List(
          CmpInstr(loc, ImmVal(0)),
          BInstr("_errNull", EQcond),
        )
        case _ => List()
      }
    }

    /* Checks if the register is R0, R3 or R8 and allocates a new register if it is */
    def checkRegister(regInstr: (Register, List[Instruction])): (Register, List[Instruction]) = {
      val (register, instr) = regInstr
      if (register == R0 || register == R3 || register == R8) {
        allocator.allocateRegister()
      }
      (register, instr)
    }

    /* Recurively returns the register location of array element as well as instructions for loading and storing to the array */
    def getArrayElemLocation(arrayType: Type, arrayReg: Register, indices: List[Expr]): (List[Instruction], List[Instruction], Register) = {
      predefinedFunctions += arrayLoad4Func
      if (indices.isEmpty) {
        return (List(), List(), arrayReg)
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
          predefinedFunctions += arrayStoreFunc(OneByte)
          "_arrStore1"
        case _ => 
          predefinedFunctions += arrayStoreFunc(FourBytes)
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
      (beforeLines.toList, afterLines.toList, target)
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
    def readGenerate(lvalue: LValue): List[Instruction] = {
      val _type = lvalue.getType match {
        case BaseT("int") =>
          predefinedFunctions += readIntFunc
          "i"
        case BaseT("char") =>
          predefinedFunctions += readCharFunc
          "c"
        case _ => ""
      }
      val (beforeLines, afterLines, target): (List[Instruction], List[Instruction], Register) = getLvalueLocation(lvalue)
      Comment("Start of read statement", 4) ::
      beforeLines ++
      List(
        Mov(dest, target),
        Mov(R0, dest),
        BInstr(s"_read${_type}", noCondition, storeReturnAddr = true),
        Mov(target, R0)
      ) ++
      afterLines ++
      List(Comment("End of read statement", 4))
    }

    /* Generates instructions for a free statement */
    def ifGenerate(cond: Expr, thenS: Statement, elseS: Statement): List[Instruction] = {
      val elseLabel = getUniqueLabel
      val endLabel = getUniqueLabel
      val condLines = generateInstructions(cond, allocator, dest)
      val thenLines = generateInstructions(thenS, allocator, dest)
      val elseLines = generateInstructions(elseS, allocator, dest)
      Comment("Start of if statement", 4) ::
      condLines ++
      List(
        CmpInstr(dest, ImmVal(1)),
        BInstr(elseLabel, NEcond)
      ) ++
      thenLines ++
      List(
        BInstr(endLabel),
        Label(elseLabel)
      ) ++
      elseLines ++
      List(Label(endLabel), Comment("End of if statement", 4))
    }

    /* Generates instructions for a while statement */
    def whileGenerate(cond: Expr, stmt: Statement): List[Instruction] = {
      val startLabel = getUniqueLabel
      val endLabel = getUniqueLabel
      val condLines = generateInstructions(cond, allocator, dest)
      val (newDest, rLines) = allocator.allocateRegister()
      val stmtLines = generateInstructions(stmt, allocator, newDest)
      Comment("Start of while loop", 4) ::
      Label(startLabel) ::
      condLines ++
      List(
        CmpInstr(dest, ImmVal(1)), 
        BInstr(endLabel, NEcond)
      ) ++ 
      rLines ++
      stmtLines ++
      List(
        BInstr(startLabel),
        Label(endLabel),
        Comment("End of while loop", 4)
      )
    }

    /* Generates instructions for a new scope */
    def scopeGenerate(body: Statement): List[Instruction] = {
      val bodyLines = generateInstructions(body, allocator, dest)
      Comment("Start of new scope", 4) ::
      bodyLines ++
      List(Comment("End of new scope", 4))
    }

    /* Generates instructions for a free statement */
    def freeGenerate(exp: Expr): List[Instruction] = {
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
      Comment("Start of free statement", 4) ::
      expLines ++
      tLines ++
      List(
        Mov(R0, dest),
        BInstr("_free" + _type, noCondition, storeReturnAddr = true),
        Comment("End of free statement", 4)
      )
    }

    /* Generates instructions for a return statement */
    def returnGenerate(exp: Expr): List[Instruction] = {
      val expLines = generateInstructions(exp, allocator, dest)
      Comment("Start of return statement", 4) ::
      expLines ++
      List(
        Mov(R0, dest),
        Mov(SP, FP),
        Pop(List(R4, R5, R6, R7, R8, R9, R10)),
        Pop(List(FP, PC)),
        Comment("End of return statement", 4)
      )
    }

    /* Generates instructions for an exit statement */
    def exitGenerate(exp: Expr): List[Instruction] = {
      predefinedFunctions += exitFunc
      val expLines = generateInstructions(exp, allocator, dest)
      Comment("Start of exit statement", 4) ::
      expLines ++
      List(
        Mov(R0, dest),
        BInstr("_exit", noCondition, storeReturnAddr = true),
        Mov(SP, FP),
        Pop(List(FP, PC)),
        Comment("End of exit statement", 4)
      )
    }

    /* Generates instructions for a print statement */
    def printGenerate(exp: Expr): List[Instruction] = {
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
      val expLines = generateInstructions(exp, allocator, dest)
      Comment("Start of print statement", 4) ::
      expLines ++
      List(
        Mov(R0, dest),
        BInstr(s"_print$t", noCondition, storeReturnAddr = true),
        Comment("End of print statement", 4)
      )
    }

    /* Generates instructions for a println statement */
    def printlnGenerate(exp: Expr): List[Instruction] = {
      predefinedFunctions += printLnFunc
      Comment("Start of println statement", 4) ::
      printGenerate(exp) ++
      List(
        BInstr("_println", noCondition, storeReturnAddr = true),
        Comment("End of println statement", 4)
      )
    }

    /* Generates instructions to create an array literal */
    def arrayLiterGenerate(elems: List[Expr]): List[Instruction] = {
      predefinedFunctions += mallocFunc
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
        arrayLines ++= List(
          Mov(dest, next), 
          StrInstr(dest, Addr(pointer, ImmVal(totalSize - size)), sizeToStore)
        )
        allocator.deallocateRegister(next)
      }
      totalSize += 4
      allocator.deallocateRegister(pointer)
      Comment("Start of array literal creation", 4) ::
      r1Lines ++
      List(
        Mov(R0, ImmVal(totalSize)),
        BInstr("_malloc", noCondition, storeReturnAddr = true),
        Mov(pointer, R0),
        AddInstr(pointer, pointer, ImmVal(4)),
        Mov(dest, ImmVal(elems.length)),
        StrInstr(dest, Addr(pointer, ImmVal(-4))),
      ) ++
      arrayLines.toList ++
      List(
        Mov(dest, pointer),
        Comment("End of array literal creation", 4)
      )
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
    def newPairGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
      predefinedFunctions += mallocFunc
      val (next, rLines) = allocator.allocateRegister()
      val newPairLines = Comment("Start of new pair creation", 4) ::
      rLines ++
      List(
        Mov(R0, ImmVal(8)),
        BInstr("_malloc", noCondition, storeReturnAddr = true),
        Mov(dest, R0),
      ) ++
      generateInstructions(exp1, allocator, next) ++
      List(
        StrInstr(next, Addr(dest, ImmVal(0)))
      ) ++
      generateInstructions(exp2, allocator, next) ++
      List(
        StrInstr(next, Addr(dest, ImmVal(4))),
        Comment("End of new pair creation", 4)
      )
      allocator.deallocateRegister(next)
      newPairLines
    }

    /* Generates instructions for an expression */
    def getExpLines (exp: Either[LValue, Expr]): (List[Instruction], List[Instruction], Register) = {
      exp.fold(
        x => generateExpLines(x),
        x => generateExpLines(x)
      )
    }

    /* Generates instructions for an expression */
    def generateExpLines(x: ASTNode): (List[Instruction], List[Instruction], Register) = {
      val (next, rLines) = allocator.allocateRegister()
      val expLines = generateInstructions(x, allocator, next)
      allocator.deallocateRegister(next)
      (expLines, rLines, next)
    }
 
    /* Generates instructions for extracting a pair element */
    def pairElemGenerate(func: String, lvalue: LValue): List[Instruction] = {
      predefinedFunctions += errorNullFunc
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
      Comment("Start of pair element extraction", 4) ::
      rLines ++
      lvalueLines ++
      List(
        CmpInstr(next, ImmVal(0)),
        BInstr("_errNull", EQcond)
      ) ++
      funcLines ++
      List(Comment("End of pair element extraction", 4))
    }

    /* Generates instructions to evaluate two expressions */
    def addMulGenerate(exp1: Expr, exp2: Expr): (List[Instruction], List[Instruction]) = {
      predefinedFunctions += errorOverflowFunc
      val exp1Lines = generateInstructions(exp1, allocator, dest)
      val exp2Lines = generateInstructions(exp2, allocator, dest)
      (exp1Lines, exp2Lines)
    }

    /* Generates instructions for a multiplication expression */
    def mulGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
      val (exp1Lines, exp2Lines) = addMulGenerate(exp1, exp2)
      val (val1, r1Lines) = allocator.allocateRegister()
      val (val2, r2Lines) = allocator.allocateRegister()
      val (hi, r3Lines) = allocator.allocateRegister()
      allocator.deallocateRegister(hi)
      allocator.deallocateRegister(val1)
      allocator.deallocateRegister(val2)

      Comment("Start of multiplication expression", 4) ::
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
        CmpInstr(hi, RegShift(dest, ShiftRight(31))),
        BInstr("_errOverflow", NEcond, storeReturnAddr = true),
        Comment("End of multiplication expression", 4)
      )
    }

    /* Generates instructions for a division or modulo expression */
    def divModGenerate(exp1: Expr, exp2: Expr, reg: Register): List[Instruction] = {
      predefinedFunctions += errorDivByZeroFunc
      val exp1Lines = generateInstructions(exp1, allocator, dest)
      val exp2Lines = generateInstructions(exp2, allocator, dest)

      exp1Lines ++
      List(Mov(R0, dest)) ++
      exp2Lines ++
      List(
        Mov(R1, dest),
        CmpInstr(R1, ImmVal(0)),
        BInstr("_errDivZero", EQcond, storeReturnAddr = true),
        BInstr("__aeabi_idivmod", noCondition, storeReturnAddr = true),
        Mov(dest, reg)
      )
    }

    /* Generates instructions for a division expression */
    def divGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
      Comment("Start of division expression", 4) ::
      divModGenerate(exp1, exp2, R0) ++
      List(Comment("End of division expression", 4))
    }

    /* Generates instructions for a modulo expression */
    def modGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
      Comment("Start of modulo expression", 4) ::
      divModGenerate(exp1, exp2, R1) ++
      List(Comment("End of modulo expression", 4))
    }

    /* Generates instructions for an addition expression */
    def addGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
      val (exp1Lines, exp2Lines) = addMulGenerate(exp1, exp2)
      val (next, rLines) = allocator.allocateRegister()
      allocator.deallocateRegister(next)
      Comment("Start of addition expression", 4) ::
      exp2Lines ++
      List(Push(List(dest))) ++
      exp1Lines ++
      rLines ++ 
      List(
        Pop(List(next)),
        AddInstr(dest, dest, next, updateFlags = true),
        BInstr("_errOverflow", VScond, storeReturnAddr = true),
        Comment("End of addition expression", 4)
      )
    }

    /* Generates instructions to evaluate exp1 into dest and exp2 into next */
    def exprsGenerateHelper(exp1: Expr, exp2: Expr): (List[Instruction], List[Instruction], List[Instruction], Register) = {
      val exp1Lines = generateInstructions(exp1, allocator, dest)
      val (next, rLines) = allocator.allocateRegister()
      val exp2Lines = generateInstructions(exp2, allocator, next)
      allocator.deallocateRegister(next)
      (exp1Lines, rLines, exp2Lines, next)
    }

    /* Generates instructions for a subtraction expression */
    def subGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
      predefinedFunctions += errorOverflowFunc
      val (exp1Lines, rLines, exp2Lines, next) = exprsGenerateHelper(exp1, exp2)
      Comment("Start of subtraction expression", 4) ::
      exp1Lines ++
      rLines ++ 
      exp2Lines ++
      List(
        SubInstr(dest, dest, next, updateFlags = true),
        BInstr("_errOverflow", VScond, storeReturnAddr = true),
        Comment("End of subtraction expression", 4)
      )
    }

    /* Generates instructions for a conditional expression */
    def condGenerate(exp1: Expr, exp2: Expr, cond: Condition): List[Instruction] = {
      val (exp1Lines, rLines, exp2Lines, next) = exprsGenerateHelper(exp1, exp2)
      exp1Lines ++
      rLines ++
      exp2Lines ++
      List(
        CmpInstr(dest, next),
        Mov(dest, ImmVal(0)),
        Mov(dest, ImmVal(1), cond)
      )
    }

    /* Generates instructions for an and expression */
    def andGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
      val (exp1Lines, rLines, exp2Lines, next) = exprsGenerateHelper(exp1, exp2)
      Comment("Start of and expression", 4) ::
      exp1Lines ++
      rLines ++
      exp2Lines ++
      List(
        CmpInstr(next, ImmVal(0)),
        Mov(dest, ImmVal(0), EQcond),
        Comment("End of and expression", 4)
      )
    }

    /* Generates instructions for an or expression */
    def orGenerate(exp1: Expr, exp2: Expr): List[Instruction] = {
      val (exp1Lines, rLines, exp2Lines, next) = exprsGenerateHelper(exp1, exp2)
      Comment("Start of or expression", 4) ::
      exp1Lines ++
      rLines ++
      exp2Lines ++
      List(
        CmpInstr(next, ImmVal(1)),
        Mov(dest, ImmVal(1), EQcond),
        Comment("End of or expression", 4)
      )
    }

    /* Generates instructions for a not expression */
    def notGenerate(exp: Expr): List[Instruction] = {
      val expLines = generateInstructions(exp, allocator, dest)
      Comment("Start of not expression", 4) ::
      expLines ++
      List(
        CmpInstr(dest, ImmVal(0)),
        Mov(dest, ImmVal(0), NEcond),
        Mov(dest, ImmVal(1), EQcond),
        Comment("End of not expression", 4)
      )
    }

    /* Generates instructions for a negation expression */
    def negGenerate(exp: Expr): List[Instruction] = {
      predefinedFunctions += errorOverflowFunc
      val (expLines, rLines, tempReg) = getExpLines(Right(exp))
      Comment("Start of negation expression", 4) ::
      rLines ++ 
      expLines ++
      List(
        RsbsInstr(dest, tempReg, ImmVal(0)),
        BInstr("_errOverflow", VScond, storeReturnAddr = true),
        Comment("End of negation expression", 4)
      )
    }

    /* Generates instructions for a length expression */
    def lenGenerate(exp: Expr): List[Instruction] = {
      val (next, rLines) = allocator.allocateRegister()
      val expLines = generateInstructions(exp, allocator, next)
      allocator.deallocateRegister(next)
      Comment("Start of length expression", 4) ::
      rLines ++ 
      expLines ++
      List(
        Ldr(dest, Addr(next, ImmVal(-4))),
        Comment("End of length expression", 4)
      )
    }

    /* Generates instructions for an ord expression */
    def ordGenerate(exp: Expr): List[Instruction] = {
      val expLines = generateInstructions(exp, allocator, dest)
      Comment("Start of ord expression", 4) ::
      expLines ++
      List(Comment("End of ord expression", 4))
    }

    /* Generates instructions for a chr expression */
    def chrGenerate(exp: Expr): List[Instruction] = {
      predefinedFunctions += errorBadCharFunc
      val (next, rLines) = allocator.allocateRegister()
      val expLines = generateInstructions(exp, allocator, dest)
      Comment("Start of chr expression", 4) ::
      expLines ++
      rLines ++
      List(
        Mov(next, ImmVal(-128)),
        Tst(dest, next), 
        Mov(R1, dest, NEcond),
        BInstr("_errBadChar", NEcond, storeReturnAddr = true),
        Comment("End of chr expression", 4)
      )
    }

    /* Generates instructions for loading a number into dest */
    def numGenerate(n: Int): List[Instruction] = {
      val instr = if (n < 0 || n > 255) {
        Ldr(dest, IntLiteral(n))
      } else {
        Mov(dest, ImmVal(n))
      }

      List(
        Comment("Load number", 4),
        instr
      )
    }

    /* Generates instructions for loading a boolean into dest */
    def boolGenerate(b: String): List[Instruction] = {
      Comment("Load boolean", 4) ::
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

    /* Generates instructions for loading a character into dest */
    def chGenerate(c: Char): List[Instruction] = {
      List(
        Comment("Load character", 4),
        Mov(dest, ImmVal(c.toInt))
      )
    }

    /* Generates instructions for loading an identifier into dest */
    def identGenerate(n: String): List[Instruction] = {
      val location: Option[VariableLocation] = allocator.lookupLocation(n)
      Comment("Load identifier", 4) ::
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
      })
    }

    /* Generates instructions for loading a string into dest */
    def strGenerate(s: String): List[Instruction] = {
      val label = getStringLabel
      val asciz = AscizInstr(label, StringLiteral(s))
      stringPool += asciz
      Comment("Load string", 4) ::
      List(
        Ldr(dest, LabelAddr(label))
      )
    }

    /* Generates instructions for loading an array element into dest */
    def arrayElemGenerate(id: Ident, indices: List[Expr]) = {
      predefinedFunctions += arrayLoad4Func
      /* Load the array pointer into the dest register */
      val idLines = generateInstructions(id, allocator, dest)
      val indicesLines = new ListBuffer[Instruction]()
      /* For each index in the list of indices, load the array element into the dest register */
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
      Comment("Start of array element extraction", 4) ::
      idLines ++
      indicesLines.toList ++
      List(Comment("End of array element extraction", 4))
    }

    /* Generates instructions for a function call */
    def callGenerate(funcName: Ident, args: List[Expr]): List[Instruction] = {
      Comment("Start of function call", 4) ::
      Push(List(R1, R2, R3)) ::
      argsGenerate(args) ++
      List(
        BInstr(funcName.nickname.get, noCondition, storeReturnAddr = true),
        Pop(List(R1, R2, R3)),
        Mov(dest, R0),
        Comment("End of function call", 4)
      )
    }

    /* Generates instructions for function call arguments */
    def argsGenerate(args: List[Expr]): List[Instruction] = {
      val argsLines = new ListBuffer[Instruction]()
      /* For each argument in the list of arguments, store the argument in the correct place */
      for (i <- args.indices) {
        val arg = args(i)
        val (next, rLines) = allocator.allocateRegister()
        val argLines = generateInstructions(arg, allocator, next)
        argsLines ++= rLines
        argsLines ++= argLines
        allocator.deallocateRegister(next)
        /* The first 4 arguments ar stored in R0-3 */
        i match {
          case 0 =>
            argsLines += Mov(R0, next)
          case 1 =>
            argsLines += Mov(R1, next)
          case 2 =>
            argsLines += Mov(R2, next)
          case 3 =>
            argsLines += Mov(R3, next)
          /* The rest of the arguments are stored on the stack */
          case _ => argsLines += StrInstr(next, Addr(SP, ImmVal(4 * (args.length - i - 1))))
        }
      }
      argsLines.toList
    }

    /* Identifies the ast node and generates the corresponding instructions */
    astNode match {
      case Program(funcs, stmts) =>
        programGenerate(funcs, stmts)

      case Function(_type, funcName, paramList, body) =>
        functionGenerate(_type, funcName, paramList, body)

      case Skip() =>
        List(Comment("Skip", 4))

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
        /* Generates instructions for each statement in the list of statements and concatenates them */
        val stmtLines = stmts.flatMap(generateInstructions(_, allocator, dest))
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
        List(
          Comment("Load pair literal", 4),
          Mov(dest, ImmVal(0))
        )

      case Ident(_, n, _) =>
        n match {
          case Some(v) => identGenerate(v)
          case None => List()
        }

      case ArrayElem(f, p) =>
        arrayElemGenerate(f, p)

      case _ => List()
    }
  }
}
