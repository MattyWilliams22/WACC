package wacc.backend

import wacc.ASTNodes._
import wacc.backend.Instructions._

object CodeGenerator {
  var labelCounter = -1

  def getUniqueLabel(): String = {
    labelCounter += 1
    "L" + labelCounter
  }

  def generateAssembly(ast: ASTNode, allocator: RegisterAllocator): List[AssemblyLine] = {

    ast match {
      case Program(funcs, stmts) =>
        val funcLines = funcs.flatMap(generateAssembly(_, allocator))
        val stmtLines = generateAssembly(stmts, allocator)
        Comment("Start of program") :: funcLines ++
          List(Label("main"), PushMultiple(List(FP, LR)), Mov(FP, SP)) ++
          stmtLines ++
          List(PopMultiple(List(FP, PC)))

      case Function(_, funcName, paramList, body) =>
        val paramLines = paramList.flatMap(generateAssembly(_, allocator))
        val bodyLines = generateAssembly(body, allocator)
        Comment("Start of function") :: 
        Label(funcName.nickname.get) :: 
        PushMultiple(List(FP, LR)) ::
        Mov(FP, SP) ::
        paramLines ++ 
        bodyLines ++ 
        List(PopMultiple(List(FP, PC)))

      case Param(_, _) =>
        List(Comment("Start of parameter"))

      case Skip() =>
        List(Comment("Skip"), Mov(allocator.allocateRegister(), ImmVal(0)))

      case Declare(_, _, value) =>
        val valueLines = generateAssembly(value, allocator)
        Comment("Start of declare") ::
        valueLines ++
        List(Comment("Declare Logic"))

      case Assign(_, rvalue) =>
        val rvalueLines = generateAssembly(rvalue, allocator)
        Comment("Start of assign") :: 
        rvalueLines ++
        List(Comment("Assign Logic"))

      case Read(lvalue) =>
        val lvalueLines = generateAssembly(lvalue, allocator)
        Comment("Start of read") ::
        lvalueLines ++
        List(Comment("Read Logic"))

      case If(cond, thenS, elseS) =>
        val elseLabel = getUniqueLabel()
        val endLabel = getUniqueLabel()
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

      case While(cond, stmt) =>
        val startLabel = getUniqueLabel()
        val endLabel = getUniqueLabel()
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

      case Scope(body) =>
        val bodyLines = generateAssembly(body, allocator)
        Comment("Start of new scope") ::
        bodyLines ++
        List(Comment("End of new scope"))

      case Statements(stmts) =>
        val stmtLines = stmts.flatMap(generateAssembly(_, allocator))
        stmtLines

      case Free(exp) =>
        val expLines = generateAssembly(exp, allocator)
        Comment("Start of free") :: 
        expLines ++
        List(Comment("Free Logic"))

      case Return(exp) =>
        val expLines = generateAssembly(exp, allocator)
        Comment("Start of return") ::
        expLines ++
        List(
          Comment("Return Logic"),
          RetInstr()
        )

      case Exit(exp) =>
        val expLines = generateAssembly(exp, allocator)
        Comment("Start of exit") ::
        expLines ++
        List(Comment("Exit Logic"))

      case Print(exp) =>
        val expLines = generateAssembly(exp, allocator)
        Comment("Start of print") :: 
        expLines ++
        List(Comment("Print Logic"))

      case Println(exp) =>
        val expLines = generateAssembly(exp, allocator)
        Comment("Start of println") ::
        expLines ++
        List(Comment("Println Logic"))

      case ArrayLiter(elems) =>
        val elemLines = elems.flatMap(generateAssembly(_, allocator))
        Comment("Start of array literal") ::
        elemLines ++
        List(Comment("ArrayLiter Logic"))

      case NewPair(exp1, exp2) =>
        val exp1Lines = generateAssembly(exp1, allocator)
        val exp2Lines = generateAssembly(exp2, allocator)
        Comment("Start of new pair") ::
        exp1Lines ++
        exp2Lines ++
        List(Comment("NewPair Logic"))

      case PairElem(_, lvalue) =>
        val lvalueLines = generateAssembly(lvalue, allocator)
        Comment("Start of pair element") :: 
        lvalueLines ++
        List(Comment("PairElem Logic"))

      case Call(funcName, _) =>
        List(
          Comment("Start of function call"),
          BlInstr(funcName.nickname.get)
        )

      case x: BinOp =>
        x match {
          case Mul(exp1, exp2) =>
            val exp1Lines = generateAssembly(exp1, allocator)
            val exp2Lines = generateAssembly(exp2, allocator)
            Comment("Start of multiplication") ::
            exp1Lines ++
            exp2Lines ++
            List(MulInstr(allocator.allocateRegister(), allocator.allocateRegister(), allocator.allocateRegister()))

          case Div(exp1, exp2) =>
            val exp1Lines = generateAssembly(exp1, allocator)
            val exp2Lines = generateAssembly(exp2, allocator)
            Comment("Start of division") ::
            exp1Lines ++
            exp2Lines ++
            List(DivInstr(allocator.allocateRegister(), allocator.allocateRegister(), allocator.allocateRegister()))

          case Mod(exp1, exp2) =>
            val exp1Lines = generateAssembly(exp1, allocator)
            val exp2Lines = generateAssembly(exp2, allocator)
            Comment("Start of modulo") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("Mod Logic"))

          case Add(exp1, exp2) =>
            val exp1Lines = generateAssembly(exp1, allocator)
            val exp2Lines = generateAssembly(exp2, allocator)
            Comment("Start of addition") ::
            exp1Lines ++
            exp2Lines ++
            List(AddInstr(allocator.allocateRegister(), allocator.allocateRegister(), allocator.allocateRegister()))

          case Sub(exp1, exp2) =>
            val exp1Lines = generateAssembly(exp1, allocator)
            val exp2Lines = generateAssembly(exp2, allocator)
            Comment("Start of subtraction") ::
            exp1Lines ++
            exp2Lines ++
            List(SubInstr(allocator.allocateRegister(), allocator.allocateRegister(), allocator.allocateRegister()))
        }

      case x: BinOpCompare =>
        x match {
          case GT(exp1, exp2) =>
            val exp1Lines = generateAssembly(exp1, allocator)
            val exp2Lines = generateAssembly(exp2, allocator)
            Comment("Start of greater than") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("GT Logic"))

          case GTEQ(exp1, exp2) =>
            val exp1Lines = generateAssembly(exp1, allocator)
            val exp2Lines = generateAssembly(exp2, allocator)
            Comment("Start of greater than or equal to") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("GTEQ Logic"))

          case LT(exp1, exp2) =>
            val exp1Lines = generateAssembly(exp1, allocator)
            val exp2Lines = generateAssembly(exp2, allocator)
            Comment("Start of less than") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("LT Logic"))

          case LTEQ(exp1, exp2) =>
            val exp1Lines = generateAssembly(exp1, allocator)
            val exp2Lines = generateAssembly(exp2, allocator)
            Comment("Start of less than or equal to") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("LTEQ Logic"))
        }

      case x: Equality =>
        x match {
          case EQ(exp1, exp2) =>
            val exp1Lines = generateAssembly(exp1, allocator)
            val exp2Lines = generateAssembly(exp2, allocator)
            Comment("Start of equality") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("EQ Logic"))

          case NEQ(exp1, exp2) =>
            val exp1Lines = generateAssembly(exp1, allocator)
            val exp2Lines = generateAssembly(exp2, allocator)
            Comment("Start of not equal to") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("NEQ Logic"))
        }

      case x: BinOpLogic =>
        x match {
          case And(exp1, exp2) =>
            val exp1Lines = generateAssembly(exp1, allocator)
            val exp2Lines = generateAssembly(exp2, allocator)
            Comment("Start of and") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("and Logic"))

          case Or(exp1, exp2) =>
            val exp1Lines = generateAssembly(exp1, allocator)
            val exp2Lines = generateAssembly(exp2, allocator)
            Comment("Start of or") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("or Logic"))
        }

      case Not(exp) =>
        val expLines = generateAssembly(exp, allocator)
        Comment("Start of not") ::
        expLines ++
        List(Comment("not Logic"))

      case Neg(exp) =>
        val expLines = generateAssembly(exp, allocator)
        Comment("Start of negation") ::
        expLines ++
        List(Comment("neg Logic"))

      case Len(exp) =>
        val expLines = generateAssembly(exp, allocator)
        Comment("Start of length") ::
        expLines ++
        List(Comment("len Logic"))

      case Ord(exp) =>
        val expLines = generateAssembly(exp, allocator)
        Comment("Start of ord") ::
        expLines ++
        List(Comment("ord Logic"))

      case Chr(exp) =>
        val expLines = generateAssembly(exp, allocator)
        Comment("Start of chr") ::
        expLines ++
        List(Comment("chr Logic"))

      case Num(n) =>
        List(
          Comment("Start of number"),
          Mov(allocator.allocateRegister(), ImmVal(n))
        )

      case Bool(b) =>
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

      case Ch(c) =>
        List(
          Comment("Start of character"),
          Mov(allocator.allocateRegister(), ImmVal(c.toInt))
        )

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
