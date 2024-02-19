package wacc.backend

import wacc.ASTNodes._
import wacc.backend.Instructions._

object CodeGenerator {
  var labelCounter: Int = -1

  private def getUniqueLabel: String = {
    labelCounter += 1
    "L" + labelCounter
  }

  def generateAssembly(ast: ASTNode, allocator: RegisterAllocator): List[AssemblyLine] = {

     def programGenerate(funcs: List[Function], stmts: Statement): List[AssemblyLine] = {
      val funcLines = funcs.flatMap(generateAssembly(_, allocator))
      val stmtLines = generateAssembly(stmts, allocator)
      Comment("Start of program") :: funcLines ++
      List(Label("main"), PushMultiple(List(FP, LR)), Mov(FP, SP)) ++
      stmtLines ++
      List(PopMultiple(List(FP, PC)))
    }

    def functionGenerate(funcName: Ident, paramList: List[Param], body: Statement): List[AssemblyLine] = {
      val paramLines = paramList.flatMap(generateAssembly(_, allocator))
      val bodyLines = generateAssembly(body, allocator)
      Comment("Start of function") ::
      Label(funcName.nickname.get) ::
      PushMultiple(List(FP, LR)) ::
      Mov(FP, SP) ::
      paramLines ++
      bodyLines ++
      List(PopMultiple(List(FP, PC)))
    }

    def declareGenerate(value: RValue): List[AssemblyLine] = {
      val valueLines = generateAssembly(value, allocator)
      Comment("Start of declare") ::
      valueLines ++
      List(Comment("Declare Logic"))
    }

    def assignGenerate(rvalue: RValue): List[AssemblyLine] = {
      val rvalueLines = generateAssembly(rvalue, allocator)
      Comment("Start of assign") ::
      rvalueLines ++
      List(Comment("Assign Logic"))
    }

    def readGenerate(lvalue: LValue): List[AssemblyLine] = {
      val lvalueLines = generateAssembly(lvalue, allocator)
      Comment("Start of read") ::
      lvalueLines ++
      List(Comment("Read Logic"))
    }

    def ifGenerate(cond: Expr, thenS: Statement, elseS: Statement): List[AssemblyLine] = {
      val elseLabel = getUniqueLabel
      val endLabel = getUniqueLabel
      val condLines = generateAssembly(cond, allocator)
      val thenLines = generateAssembly(thenS, allocator)
      val elseLines = generateAssembly(elseS, allocator)
      Comment("Start of if statement") ::
      condLines ++
      List(Comment("If statement condition logic")) ++
      thenLines ++
      List(BInstr(endLabel), Label(elseLabel)) ++
      elseLines ++
      List(Label(endLabel), Comment("End of if statement"))
    }

    def whileGenerate(cond: Expr, stmt: Statement): List[AssemblyLine] = {
      val startLabel = getUniqueLabel
      val endLabel = getUniqueLabel
      val condLines = generateAssembly(cond, allocator)
      val stmtLines = generateAssembly(stmt, allocator)
      List(Comment("Start of while loop"))
      Label(startLabel) ::
      condLines ++
      List(Comment("While loop condition logic"), BInstr(endLabel)) ++ // Temporary jump to end label
      stmtLines ++
      List(
        BInstr(startLabel),
        Label(endLabel),
        Comment("End of while loop")
      )
    }

    def scopeGenerate(body: Statement): List[AssemblyLine] = {
      val bodyLines = generateAssembly(body, allocator)
      Comment("Start of new scope") ::
      bodyLines ++
      List(Comment("End of new scope"))
    }

    def freeGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator)
      Comment("Start of free") ::
      expLines ++
      List(Comment("Free Logic"))
    }

    def returnGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator)
      Comment("Start of return") ::
      expLines ++
      List(
        Comment("Return Logic"),
        RetInstr()
      )
    }

    def exitGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator)
      Comment("Start of exit") ::
      expLines ++
      List(Comment("Exit Logic"))
    }

    def printGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator)
      Comment("Start of print") ::
      expLines ++
      List(Comment("Print Logic"))
    }

    def printlnGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator)
      Comment("Start of println") ::
      expLines ++
      List(Comment("Println Logic"))
    }

    def arrayLiterGenerate(elems: List[Expr]): List[AssemblyLine] = {
      val elemLines = elems.flatMap(generateAssembly(_, allocator))
      Comment("Start of array literal") ::
      elemLines ++
      List(Comment("ArrayLiter Logic"))
    }

    def newPairGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator)
      val exp2Lines = generateAssembly(exp2, allocator)
      Comment("Start of new pair") ::
      exp1Lines ++
      exp2Lines ++
      List(Comment("NewPair Logic"))
    }

    def pairElemGenerate(lvalue: LValue): List[AssemblyLine] = {
      val lvalueLines = generateAssembly(lvalue, allocator)
      Comment("Start of pair element") ::
      lvalueLines ++
      List(Comment("PairElem Logic"))
    }

    def mulGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator)
      val exp2Lines = generateAssembly(exp2, allocator)
      Comment("Start of multiplication") ::
      exp1Lines ++
      exp2Lines ++
      List(MulInstr(allocator.allocateRegister(), allocator.allocateRegister(), allocator.allocateRegister()))
    }

    def divGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator)
      val exp2Lines = generateAssembly(exp2, allocator)
      Comment("Start of division") ::
      exp1Lines ++
      exp2Lines ++
      List(DivInstr(allocator.allocateRegister(), allocator.allocateRegister(), allocator.allocateRegister()))
    }

    def modGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator)
      val exp2Lines = generateAssembly(exp2, allocator)
      Comment("Start of modulo") ::
      exp1Lines ++
      exp2Lines ++
      List(Comment("Mod Logic"))
    }

    def addGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator)
      val exp2Lines = generateAssembly(exp2, allocator)
      Comment("Start of addition") ::
      exp1Lines ++
      exp2Lines ++
      List(AddInstr(allocator.allocateRegister(), allocator.allocateRegister(), allocator.allocateRegister()))
    }

    def subGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator)
      val exp2Lines = generateAssembly(exp2, allocator)
      Comment("Start of subtraction") ::
      exp1Lines ++
      exp2Lines ++
      List(SubInstr(allocator.allocateRegister(), allocator.allocateRegister(), allocator.allocateRegister()))
    }

    def gtGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator)
      val exp2Lines = generateAssembly(exp2, allocator)
      Comment("Start of greater than") ::
      exp1Lines ++
      exp2Lines ++
      List(Comment("GT Logic"))
    }

    def gteqGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator)
      val exp2Lines = generateAssembly(exp2, allocator)
      Comment("Start of greater than or equal to") ::
      exp1Lines ++
      exp2Lines ++
      List(Comment("GTEQ Logic"))
    }

    def ltGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator)
      val exp2Lines = generateAssembly(exp2, allocator)
      Comment("Start of less than") ::
      exp1Lines ++
      exp2Lines ++
      List(Comment("LT Logic"))
    }

    def lteqGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator)
      val exp2Lines = generateAssembly(exp2, allocator)
      Comment("Start of less than or equal to") ::
      exp1Lines ++
      exp2Lines ++
      List(Comment("LTEQ Logic"))
    }

    def eqGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator)
      val exp2Lines = generateAssembly(exp2, allocator)
      Comment("Start of equality") ::
      exp1Lines ++
      exp2Lines ++
      List(Comment("EQ Logic"))
    }

    def neqGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator)
      val exp2Lines = generateAssembly(exp2, allocator)
      Comment("Start of not equal to") ::
      exp1Lines ++
      exp2Lines ++
      List(Comment("NEQ Logic"))
    }

    def andGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator)
      val exp2Lines = generateAssembly(exp2, allocator)
      Comment("Start of and") ::
      exp1Lines ++
      exp2Lines ++
      List(Comment("and Logic"))
    }

    def orGenerate(exp1: Expr, exp2: Expr): List[AssemblyLine] = {
      val exp1Lines = generateAssembly(exp1, allocator)
      val exp2Lines = generateAssembly(exp2, allocator)
      Comment("Start of or") ::
      exp1Lines ++
      exp2Lines ++
      List(Comment("or Logic"))
    }

    def notGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator)
      Comment("Start of not") ::
      expLines ++
      List(Comment("not Logic"))
    }

    def negGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator)
      Comment("Start of negation") ::
      expLines ++
      List(Comment("neg Logic"))
    }

    def lenGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator)
      Comment("Start of length") ::
      expLines ++
      List(Comment("len Logic"))
    }

    def ordGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator)
      Comment("Start of ord") ::
      expLines ++
      List(Comment("ord Logic"))
    }

    def chrGenerate(exp: Expr): List[AssemblyLine] = {
      val expLines = generateAssembly(exp, allocator)
      Comment("Start of chr") ::
      expLines ++
      List(Comment("chr Logic"))
    }

    def numGenerate(n: Int): List[AssemblyLine] = {
      List(
        Comment("Start of number"),
        Mov(allocator.allocateRegister(), ImmVal(n))
      )
    }

    def boolGenerate(b: String): List[AssemblyLine] = {
      Comment("Start of boolean") ::
      (b match {
        case "true" =>
          List(
            Mov(allocator.allocateRegister(), ImmVal(1))
          )
        case "false" =>
          List(
            Mov(allocator.allocateRegister(), ImmVal(0))
          )
      })
    }

    def chGenerate(c: Char): List[AssemblyLine] = {
      List(
        Comment("Start of character"),
        Mov(allocator.allocateRegister(), ImmVal(c.toInt))
      )
    }

    ast match {
      case Program(funcs, stmts) =>
        programGenerate(funcs, stmts)

      case Function(_, funcName, paramList, body) =>
        functionGenerate(funcName, paramList, body)

      case Param(_, _) =>
        List(Comment("Start of parameter"))

      case Skip() =>
        List(Comment("Skip"), Mov(allocator.allocateRegister(), ImmVal(0)))

      case Declare(_, _, value) =>
        declareGenerate(value)

      case Assign(_, rvalue) =>
        assignGenerate(rvalue)

      case Read(lvalue) =>
        readGenerate(lvalue)

      case If(cond, thenS, elseS) =>
        ifGenerate(cond, thenS, elseS)

      case While(cond, stmt) =>
        whileGenerate(cond, stmt)

      case Scope(body) =>
        scopeGenerate(body)

      case Statements(stmts) =>
        val stmtLines = stmts.flatMap(generateAssembly(_, allocator))
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

      case Call(funcName, _) =>
        List(
          Comment("Start of function call"),
          BlInstr(funcName.nickname.get)
        )

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
        List(Comment("Start of string"))

      case PairLiter(_) =>
        List(Comment("Start of pair literal"))

      case Ident(_, _) =>
        List(Comment("Start of identifier"))

      case ArrayElem(_, _) =>
        List(Comment("Start of array element"))

      case _ => List()
    }
  }
}
