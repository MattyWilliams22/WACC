package wacc

object ASTNodes {

  sealed trait Expr extends RValue {
    def check(): Boolean
  }
  case class Mul(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      return (exp1.check() && exp2.check())
    }
  }
  case class Div(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      return (exp1.check() && exp2.check())
    }
  }
  case class Mod(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      return (exp1.check() && exp2.check())
    }
  }
  case class Add(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      return (exp1.check() && exp2.check())
    }
  }
  case class Sub(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      return (exp1.check() && exp2.check())
    }
  }
  case class GT(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      return (exp1.check() && exp2.check())
    }
  }
  case class GTEQ(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      return (exp1.check() && exp2.check())
    }
  }
  case class LT(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      return (exp1.check() && exp2.check())
    }
  }
  case class LTEQ(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      return (exp1.check() && exp2.check())
    }
  }
  case class EQ(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      return (exp1.check() && exp2.check())
    }
  }
  case class NEQ(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      return (exp1.check() && exp2.check())
    }
  }
  case class And(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      return (exp1.check() && exp2.check())
    }
  }
  case class Or(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      return (exp1.check() && exp2.check())
    }
  }
  case class Not(exp: Expr) extends Expr {
    def check(): Boolean = {
      return exp.check()
    }
  }
  case class Neg(exp: Expr) extends Expr {
    def check(): Boolean = {
      return exp.check()
    }
  }
  case class Len(exp: Expr) extends Expr {
    def check(): Boolean = {
      return exp.check()
    }
  }
  case class Ord(exp: Expr) extends Expr {
    def check(): Boolean = {
      return exp.check()
    }
  }
  case class Chr(exp: Expr) extends Expr {
    def check(): Boolean = {
      return exp.check()
    }
  }

  sealed trait Atom extends Expr
  case class Num(value: BigInt) extends Atom {
    def check(): Boolean = {
      return true
    }
  }
  case class Bool(bool: String) extends Atom {
    def check(): Boolean = {
      return (bool == "true" || bool == "false")
    }
  }
  case class Ch(chr: Char) extends Atom {
    def check(): Boolean = {
      return true
    }
  }
  case class Str(str: String) extends Atom {
    def check(): Boolean = {
      return true
    }
  }
  case class PairLiter() extends Atom {
    def check(): Boolean = {
      return true;
    }
  }
  case class Ident(str: String) extends Atom with LValue {
    def check(): Boolean = {
      // Need symbol table code
      return false
    }
  }
  case class ArrayElem(ident: Ident, args: List[Expr]) extends Atom with LValue {
    def check(): Boolean = {
      var valid: Boolean = true
      valid = valid && ident.check()
      for (arg <- args) {
        valid = valid && arg.check()
      }
      return valid
    }
  }

  sealed trait Type {
    def check(): Boolean
  }
  sealed trait PairElemT extends Type {
    def check(): Boolean
  }
  case class BaseT(str: String) extends Type with PairElemT {
    def check(): Boolean = {
      return (str == "int" || str == "bool" || str == "char" || str == "string")
    }
  }
  case class ArrayT(_type: Type) extends Type with PairElemT {
    def check(): Boolean = {
      return _type.check()
    }
  }
  case class PairT(pet1: PairElemT, pet2: PairElemT) extends Type {
    def check(): Boolean = {
      return (pet1.check() && pet2.check())
    }
  }
  case class PairNull() extends PairElemT {
    def check(): Boolean = {
      return true
    }
  }

  case class Prog(funcs: List[Func], stat: Stmt) {
    def check(): Boolean = {
      var valid: Boolean = true
      for (func <- funcs) {
        valid = valid && func.check()
      }
      return (valid && stat.check())
    }
  }

  case class Func(_type: Type, ident: Ident, param_list: List[Param], body: Stmt) {
    def check(): Boolean = {
      var valid: Boolean = true
      valid = valid && _type.check()
      valid = valid && ident.check()
      for (param <- param_list) {
        valid = valid && param.check()
      }
      return (valid && body.check())
    }
  }

  case class Param(_type: Type, ident: Ident) {
    def check(): Boolean = {
      return (_type.check() && ident.check())
    }
  }

  sealed trait Stmt {
    def check(): Boolean
  }
  case class Skip() extends Stmt {
    def check(): Boolean = {
      return true
    }
  }
  case class Declare(_type: Type, ident: Ident, value: RValue) extends Stmt {
    def check(): Boolean = {
      return (_type.check() && ident.check() && value.check())
    }
  }
  case class Assign(lvalue: LValue, rvalue: RValue) extends Stmt {
    def check(): Boolean = {
      return (lvalue.check() && rvalue.check())
    }
  }
  case class Read(lvalue: LValue) extends Stmt {
    def check(): Boolean = {
      return lvalue.check()
    }
  }
  case class Action(action: String, exp: Expr) extends Stmt {
    def check(): Boolean = {
      return exp.check()
    }
  }
  case class If(cond: Expr, thenS: Stmt, elseS: Stmt) extends Stmt {
    def check(): Boolean = {
      return (cond.check() && thenS.check() && elseS.check())
    }
  }
  case class While(cond: Expr, body: Stmt) extends Stmt {
    def check(): Boolean = {
      return (cond.check() && body.check())
    }
  }
  case class Scope(body: Stmt) extends Stmt {
    def check(): Boolean = {
      return body.check()
    }
  }
  case class Stmts(stmts: List[Stmt]) extends Stmt {
    def check(): Boolean = {
      var valid: Boolean = true
      for (stat <- stmts) {
        valid = valid && stat.check()
      }
      return valid
    }
  }

  sealed trait LValue {
    def check(): Boolean
  }

  sealed trait RValue {
    def check(): Boolean
  }

  case class ArrayLiter(elems: List[Expr]) extends RValue {
    def check(): Boolean = {
      var valid: Boolean = true
      for (elem <- elems) {
        valid = valid && elem.check()
      }
      return valid
    }
  }
  case class NewPair(exp1: Expr, exp2: Expr) extends RValue {
    def check(): Boolean = {
      return (exp1.check() && exp2.check())
    }
  }
  case class PairElem(func: String, lvalue: LValue) extends RValue with LValue {
    def check(): Boolean = {
      return lvalue.check()
    }
  }
  case class Call(funcName: Ident, args: List[Expr]) extends RValue {
    def check(): Boolean = {
      var valid: Boolean = true
      valid = valid && funcName.check()
      for (arg <- args) {
        valid = valid && arg.check()
      }
      return valid
    }
  }

}
