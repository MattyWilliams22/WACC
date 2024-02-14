package wacc.backend

import wacc.ASTNodes._
import wacc.backend.Instructions._

object CodeGenerator {
  
    def generateAssembly(ast: ASTNode, regs: List[Register]): List[AssemblyLine] = {
        ast match {
            case Program(funcs, stmts) => {
                val funcLines = funcs.flatMap(generateAssembly(_, regs))
                val stmtLines = generateAssembly(stmts, regs)
                Comment("Start of program") :: funcLines ++ stmtLines
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
                List(Comment("Start of declare"))
            }
            case Assign(lvalue, rvalue) => {
                List(Comment("Start of assign"))
            }
            case Read(lvalue) => {
                List(Comment("Start of read"))
            }
            case If(cond, thenS, elseS) => {
                List(Comment("Start of if statement"))
            }
            case While(cond, stmt) => {
                List(Comment("Start of while loop"))
            }
            case Scope(body) => {
                val bodyLines = generateAssembly(body, regs)
                Comment("Start of new scope") :: 
                bodyLines ++ 
                List(Comment("End of new scope"))
            }
            case Statements(stmts) => {
                val stmtLines = stmts.flatMap(generateAssembly(_, regs))
                Comment("Start of multiple statements") :: stmtLines
            }
            case Free(exp) => {
                List(Comment("Start of free"))
            }
            case Return(exp) => {
                List(Comment("Start of return"))
            }
            case Exit(exp) => {
                List(Comment("Start of exit"))
            }
            case Print(exp) => {
                List(Comment("Start of print"))
            }
            case Println(exp) => {
                List(Comment("Start of println"))
            }
            case ArrayLiter(elems) => {
                List(Comment("Start of array literal"))
            }
            case NewPair(exp1, exp2) => {
                List(Comment("Start of new pair"))
            }
            case PairElem(pair, elem) => {
                List(Comment("Start of pair element"))
            }
            case Call(funcName, args) => {
                List(Comment("Start of function call"))
            }
            case x: BinOp => {
                x match {
                    case Mul(exp1, exp2) => {
                        List(Comment("Start of multiplication"))
                    }
                    case Div(exp1, exp2) => {
                        List(Comment("Start of division"))
                    }
                    case Mod(exp1, exp2) => {
                        List(Comment("Start of modulo"))
                    }
                    case Add(exp1, exp2) => {
                        List(Comment("Start of addition"))
                    }
                    case Sub(exp1, exp2) => {
                        List(Comment("Start of subtraction"))
                    }
                }
            }
            case x: BinOpCompare => {
                x match {
                    case GT(exp1, exp2) => {
                        List(Comment("Start of greater than"))
                    }
                    case GTEQ(exp1, exp2) => {
                        List(Comment("Start of greater than or equal to"))
                    }
                    case LT(exp1, exp2) => {
                        List(Comment("Start of less than"))
                    }
                    case LTEQ(exp1, exp2) => {
                        List(Comment("Start of less than or equal to"))
                    }
                }
            }
            case x: Equality => {
                x match {
                    case EQ(exp1, exp2) => {
                        List(Comment("Start of equality"))
                    }
                    case NEQ(exp1, exp2) => {
                        List(Comment("Start of not equal to"))
                    }
                }
            }
            case x: BinOpLogic => {
                x match {
                    case And(exp1, exp2) => {
                        List(Comment("Start of and"))
                    }
                    case Or(exp1, exp2) => {
                        List(Comment("Start of or"))
                    }
                }
            }
            case Not(exp) => {
                List(Comment("Start of not"))
            }
            case Neg(exp) => {
                List(Comment("Start of negation"))
            }
            case Len(exp) => {
                List(Comment("Start of length"))
            }
            case Ord(exp) => {
                List(Comment("Start of ord"))
            }
            case Chr(exp) => {
                List(Comment("Start of chr"))
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
