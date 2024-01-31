package wacc

object ASTNodes {

  sealed trait Expr extends RValue
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
  case class Bool(bool: String) extends Atom
  case class Ch(chr: Char) extends Atom
  case class Str(str: String) extends Atom
  case class PairLiter(str: String) extends Atom
  case class Ident(str: String) extends Atom with LValue
  case class ArrayElem(ident: Ident, args: List[Expr]) extends Atom with LValue

  sealed trait Type
  sealed trait PairElemT extends Type
  case class BaseT(str: String) extends Type with PairElemT
  case class ArrayT(_type: Type) extends Type with PairElemT
  case class PairT(pet_1: PairElemT, pet_2: PairElemT) extends Type
  case class PairNull() extends PairElemT

  case class Prog(funcs: List[Func], stat: Stmt)

  case class Func(_type: Type, ident: Ident, param_list: List[Param], body: Stmt)

  case class Param(_type: Type, ident: Ident)

  sealed trait Stmt
  case class Skip() extends Stmt
  case class Declare(_type: Type, ident: Ident, value: RValue) extends Stmt
  case class Assign(lvalue: LValue, rvalue: RValue) extends Stmt
  case class Read(lvalue: LValue) extends Stmt
  case class Action(action: String, exp: Expr) extends Stmt
  case class If(cond: Expr, thenS: Stmt, elseS: Stmt) extends Stmt
  case class While(cond: Expr, body: Stmt) extends Stmt
  case class Scope(body: Stmt) extends Stmt
  case class Stmts(stmts: List[Stmt]) extends Stmt

  sealed trait LValue

  sealed trait RValue
  case class ArrayLiter(elems: List[Expr]) extends RValue
  case class NewPair(exp1: Expr, exp2: Expr) extends RValue
  case class PairElem(func: String, lvalue: LValue) extends RValue with LValue
  case class Call(funcName: Ident, args: List[Expr]) extends RValue

}
