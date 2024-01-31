package wacc

object ASTNodes {
  sealed trait Prog

  sealed trait Statement extends Prog
  case class Skip() extends Statement
  case class NewPair(exp1: Expr, exp2: Expr) extends Statement
  case class PairElem(func: String, stat: Statement) extends Statement
  case class Call(funcName: String, args: List[Expr]) extends Statement
  case class ArrayLiter(elems: List[Expr]) extends Statement
  case class Declare(_type: String, varName: String, expr: Statement) extends Statement

  sealed trait Expr extends Prog with Statement
  case class Mul(exp1: Expr, exp2: Expr) extends Expr
  case class Div(exp1: Expr, exp2: Expr) extends Expr
  case class Mod(exp1: Expr, exp2: Expr) extends Expr
  case class Add(exp1: Expr, exp2: Expr) extends Expr
  case class Sub(exp1: Expr, exp2: Expr) extends Expr
  case class GT(exp1: Expr, exp2: Expr) extends Expr
  case class GTEQ(exp1: Expr, exp2: Expr) extends Expr
  case class LT(exp1: Expr, exp2: Expr) extends Expr
  case class LTEQ(exp1: Expr, exp2: Expr) extends Expr
  case class EQ(exp1: Expr, exp2: Expr) extends Expr
  case class NEQ(exp1: Expr, exp2: Expr) extends Expr
  case class And(exp1: Expr, exp2: Expr) extends Expr
  case class Or(exp1: Expr, exp2: Expr) extends Expr
  case class Not(exp: Expr) extends Expr
  case class Neg(exp: Expr) extends Expr
  case class Len(exp: Expr) extends Expr
  case class Ord(exp: Expr) extends Expr
  case class Chr(exp: Expr) extends Expr

  sealed trait Atom extends Expr
  case class Num(value: BigInt) extends Atom
  case class Var(varName: String) extends Atom with Statement
  case class Bool(bool: String) extends Atom
  case class Ch(chr: Char) extends Atom
  case class Str(str: String) extends Atom
  case class PairLiter(str: String) extends Atom
  case class ArrayElem(varName: String, args: List[Expr]) extends Atom with Statement
}
