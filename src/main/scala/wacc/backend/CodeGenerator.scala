package wacc.backend

import wacc.ASTNodes._
import wacc.backend.Instructions._

object CodeGenerator {

  def generateAssembly(ast: ASTNode, regs: List[Register]): List[AssemblyLine] = {
    val dest = regs.head
    val tail = regs.tail
    val next = regs.tail.head
    val rest = regs.tail.tail
    ast match {
      case Program(funcs, stmts) => {
        val funcLines = funcs.flatMap(generateAssembly(_, regs))
        val stmtLines = generateAssembly(stmts, regs)
        Comment("Start of program") :: funcLines ++ List(Label("main")) ++ stmtLines
      }
      case Function(_type, ident, param_list, body) => {
        val paramLines = param_list.flatMap(generateAssembly(_, regs))
        val bodyLines = generateAssembly(body, regs)
        Comment("Start of function") :: paramLines ++ bodyLines
      }
      case Param(_type, ident) => {
        List(Comment("Start of parameter"))
      }
      case Skip() => {
        List(Comment("Skip"))
      }
      case Declare(_type, ident, value) => {
        val valueLines = generateAssembly(value, regs)
        Comment("Start of declare") ::
        valueLines ++
        List(Comment("Declare Logic"))
      }
      case Assign(lvalue, rvalue) => {
        val rvalueLines = generateAssembly(rvalue, tail)
        Comment("Start of assign") :: 
        rvalueLines ++
        List(Comment("Assign Logic"))
      }
      case Read(lvalue) => {
        val lvalueLines = generateAssembly(lvalue, regs)
        Comment("Start of read") ::
        lvalueLines ++
        List(Comment("Read Logic"))
      }
      case If(cond, thenS, elseS) => {
        val condLines = generateAssembly(cond, regs)
        val thenLines = generateAssembly(thenS, regs)
        val elseLines = generateAssembly(elseS, regs)
        Comment("Start of if statement") ::
        condLines ++
        List(Comment("If statement condition logic")) ++
        thenLines ++
        List(Jmp("ifEnd"), Label("elseS")) ++ // Need unique labels
        elseLines ++
        List(Label("ifEnd"), Comment("End of if statement"))
      }
      case While(cond, stmt) => {
        val condLines = generateAssembly(cond, regs)
        val stmtLines = generateAssembly(stmt, regs)
        Comment("Start of while loop") ::
        Label("whileStart") :: // Need unqiue label
        condLines ++
        List(Comment("While loop condition logic")) ++
        stmtLines
//        List(
//          Jmp("whileStart"),
//          Label("whileEnd"), // Need unique label
//          Comment("End of while loop")
//        )
      }
      case Scope(body) => {
        val bodyLines = generateAssembly(body, regs)
        Comment("Start of new scope") ::
        bodyLines ++
        List(Comment("End of new scope"))
      }
      case Statements(stmts) => {
        val stmtLines = stmts.flatMap(generateAssembly(_, regs))
        stmtLines
      }
      case Free(exp) => {
        val expLines = generateAssembly(exp, regs)
        Comment("Start of free") :: 
        expLines ++
        List(Comment("Free Logic"))
      }
      case Return(exp) => {
        val expLines = generateAssembly(exp, regs)
        Comment("Start of return") ::
        expLines ++
        List(
          Comment("Return Logic"),
          Ret()
        )
      }
      case Exit(exp) => {
        val expLines = generateAssembly(exp, regs)
        Comment("Start of exit") ::
        expLines ++
        List(Comment("Exit Logic"))
      }
      case Print(exp) => {
        val expLines = generateAssembly(exp, regs)
        Comment("Start of print") :: 
        expLines ++
        List(Comment("Print Logic"))
      }
      case Println(exp) => {
        val expLines = generateAssembly(exp, regs)
        Comment("Start of println") ::
        expLines ++
        List(Comment("Println Logic"))
      }
      case ArrayLiter(elems) => {
        val elemLines = elems.flatMap(generateAssembly(_, regs))
        Comment("Start of array literal") ::
        elemLines ++
        List(Comment("ArrayLiter Logic"))
      }
      case NewPair(exp1, exp2) => {
        val exp1Lines = generateAssembly(exp1, regs)
        val exp2Lines = generateAssembly(exp2, tail)
        Comment("Start of new pair") ::
        exp1Lines ++
        exp2Lines ++
        List(Comment("NewPair Logic"))
      }
      case PairElem(func, lvalue) => {
        val lvalueLines = generateAssembly(lvalue, regs)
        Comment("Start of pair element") :: 
        lvalueLines ++
        List(Comment("PairElem Logic"))
      }
      case Call(funcName, args) => {
        List(
          Comment("Start of function call"),
          CallInstr(funcName.nickname.get)
        )
      }
      case x: BinOp => {
        x match {
          case Mul(exp1, exp2) => {
            val exp1Lines = generateAssembly(exp1, regs)
            val exp2Lines = generateAssembly(exp2, tail)
            Comment("Start of multiplication") ::
            exp1Lines ++
            exp2Lines ++
            List(MulInstr(dest, next))
          }
          case Div(exp1, exp2) => {
            val exp1Lines = generateAssembly(exp1, regs)
            val exp2Lines = generateAssembly(exp2, tail)
            Comment("Start of division") ::
            exp1Lines ++
            exp2Lines ++
            List(DivInstr(dest, next))
          }
          case Mod(exp1, exp2) => {
            val exp1Lines = generateAssembly(exp1, regs)
            val exp2Lines = generateAssembly(exp2, tail)
            Comment("Start of modulo") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("Mod Logic"))
          }
          case Add(exp1, exp2) => {
            val exp1Lines = generateAssembly(exp1, regs)
            val exp2Lines = generateAssembly(exp2, tail)
            Comment("Start of addition") ::
            exp1Lines ++
            exp2Lines ++
            List(AddInstr(dest, next))
          }
          case Sub(exp1, exp2) => {
            val exp1Lines = generateAssembly(exp1, regs)
            val exp2Lines = generateAssembly(exp2, tail)
            Comment("Start of subtraction") ::
            exp1Lines ++
            exp2Lines ++
            List(SubInstr(dest, next))
          }
        }
      }
      case x: BinOpCompare => {
        x match {
          case GT(exp1, exp2) => {
            val exp1Lines = generateAssembly(exp1, regs)
            val exp2Lines = generateAssembly(exp2, tail)
            Comment("Start of greater than") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("GT Logic"))
          }
          case GTEQ(exp1, exp2) => {
            val exp1Lines = generateAssembly(exp1, regs)
            val exp2Lines = generateAssembly(exp2, tail)
            Comment("Start of greater than or equal to") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("GTEQ Logic"))
          }
          case LT(exp1, exp2) => {
            val exp1Lines = generateAssembly(exp1, regs)
            val exp2Lines = generateAssembly(exp2, tail)
            Comment("Start of less than") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("LT Logic"))
          }
          case LTEQ(exp1, exp2) => {
            val exp1Lines = generateAssembly(exp1, regs)
            val exp2Lines = generateAssembly(exp2, tail)
            Comment("Start of less than or equal to") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("LTEQ Logic"))
          }
        }
      }
      case x: Equality => {
        x match {
          case EQ(exp1, exp2) => {
            val exp1Lines = generateAssembly(exp1, regs)
            val exp2Lines = generateAssembly(exp2, tail)
            Comment("Start of equality") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("EQ Logic"))
          }
          case NEQ(exp1, exp2) => {
            val exp1Lines = generateAssembly(exp1, regs)
            val exp2Lines = generateAssembly(exp2, tail)
            Comment("Start of not equal to") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("NEQ Logic"))
          }
        }
      }
      case x: BinOpLogic => {
        x match {
          case And(exp1, exp2) => {
            val exp1Lines = generateAssembly(exp1, regs)
            val exp2Lines = generateAssembly(exp2, tail)
            Comment("Start of and") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("and Logic"))
          }
          case Or(exp1, exp2) => {
            val exp1Lines = generateAssembly(exp1, regs)
            val exp2Lines = generateAssembly(exp2, tail)
            Comment("Start of or") ::
            exp1Lines ++
            exp2Lines ++
            List(Comment("or Logic"))
          }
        }
      }
      case Not(exp) => {
        val expLines = generateAssembly(exp, regs)
        Comment("Start of not") ::
        expLines ++
        List(Comment("not Logic"))
      }
      case Neg(exp) => {
        val expLines = generateAssembly(exp, regs)
        Comment("Start of negation") ::
        expLines ++
        List(Comment("neg Logic"))
      }
      case Len(exp) => {
        val expLines = generateAssembly(exp, regs)
        Comment("Start of length") ::
        expLines ++
        List(Comment("len Logic"))
      }
      case Ord(exp) => {
        val expLines = generateAssembly(exp, regs)
        Comment("Start of ord") ::
        expLines ++
        List(Comment("ord Logic"))
      }
      case Chr(exp) => {
        val expLines = generateAssembly(exp, regs)
        Comment("Start of chr") ::
        expLines ++
        List(Comment("chr Logic"))
      }
      case Num(value) => {
        List(Comment("Start of number"))
      }
      case Bool(bool) => {
        List(Comment("Start of boolean"))
      }
      case Ch(chr) => {
        List(Comment("Start of character"))
      }
      case Str(str) => {
        List(Comment("Start of string"))
      }
      case PairLiter(str) => {
        List(Comment("Start of pair literal"))
      }
      case Ident(str, nickname) => {
        List(Comment("Start of identifier"))
      }
      case ArrayElem(ident, indices) => {
        List(Comment("Start of array element"))
      }
      case _ => List()
    }
  }
}
