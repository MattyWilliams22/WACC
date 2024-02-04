package wacc

object ASTNodes {

  case class Program(funcs: List[Function], statement: Statement) {
    def check(): Boolean = {
      var valid: Boolean = true
      for (func <- funcs) {
        valid = valid && func.check()
      }
      return valid && statement.check()
    }
  }

  case class Function(_type: Type, ident: Ident, param_list: List[Param], body: Statement) {
    def check(): Boolean = {
      var valid: Boolean = _type.check() && ident.check()
      for (param <- param_list) {
        valid = valid && param.check()
      }
      return valid && body.check()
    }

    def getType(): Type = {
      return _type.getType()
    }
  }

  case class Param(_type: Type, ident: Ident) {
    def check(): Boolean = {
      return _type.check() && ident.check()
    }

    def getType(): Type = {
      return _type.getType()
    }
  }

  sealed trait Statement {
    def check(): Boolean
  }

  case class Skip() extends Statement {
    def check(): Boolean = {
      return true
    }
  }

  case class Declare(_type: Type, ident: Ident, value: RValue) extends Statement {
    def check(): Boolean = {
      return _type.check() && ident.check() && value.check()
    }
  }

  case class Assign(lvalue: LValue, rvalue: RValue) extends Statement {
    def check(): Boolean = {
      return lvalue.check() && rvalue.check()
    }
  }

  case class Read(lvalue: LValue) extends Statement {
    def check(): Boolean = {
      return lvalue.check()
    }
  }

  case class Action(action: String, exp: Expr) extends Statement {
    def check(): Boolean = ???
  }

  case class If(cond: Expr, thenS: Statement, elseS: Statement) extends Statement {
    def check(): Boolean = {
      if (!cond.check() || !thenS.check() || !elseS.check()) {
        return false
      }
      if (cond.getType() != BaseT("bool")) {
        return false
      }
      return true
    }
  }

  case class While(cond: Expr, body: Statement) extends Statement {
    def check(): Boolean = {
      if (!cond.check() || !body.check()) {
        return false
      }
      if (cond.getType() != BaseT("bool")) {
        return false
      }
      return true
    }
  }

  case class Scope(body: Statement) extends Statement {
    def check(): Boolean = {
      return body.check()
    }
  }

  case class Statements(stmts: List[Statement]) extends Statement {
    def check(): Boolean = {
      var valid: Boolean = true
      for (stat <- stmts) {
        valid = valid && stat.check()
      }
      return valid
    }
  }

  case class Free(exp: Expr) extends Statement {
    def check(): Boolean = {
      if (!exp.check()) {
        return false
      }

      def t: Type = exp.getType()

      def valid: Boolean = t match {
        case ArrayT(_, n) if n > 0 => true
        case PairT(_, _) => true
        case _ => false
      }

      return valid
    }
  }

  case class Return(exp: Expr) extends Statement {
    def check(): Boolean = {
      return exp.check()
    }
  }

  case class Exit(exp: Expr) extends Statement {
    def check(): Boolean = {
      return exp.check()
    }
  }

  sealed trait Type {
    def check(): Boolean

    def getType(): Type
  }

  sealed trait PairElemT extends Type {
    def check(): Boolean

    def getType(): Type
  }

  case class BaseT(str: String) extends Type with PairElemT {
    def check(): Boolean = {
      return str == "int" || str == "bool" || str == "char" || str == "string"
    }

    def getType(): Type = {
      return BaseT(str)
    }
  }

  case class ArrayT(_type: Type, dim: Int) extends Type with PairElemT {
    def check(): Boolean = {
      return _type.check()
    }

    def getType(): Type = {
      return ArrayT(_type, dim)
    }
  }

  case class PairT(pet1: PairElemT, pet2: PairElemT) extends Type {
    def check(): Boolean = {
      return pet1.check() && pet2.check()
    }

    def getType(): Type = {
      return PairT(pet1, pet2)
    }
  }

  case class PairNull() extends PairElemT {
    def check(): Boolean = {
      return true
    }

    def getType(): Type = {
      return PairNull()
    }
  }

  sealed trait LValue {
    def check(): Boolean

    def getType(): Type
  }

  sealed trait RValue {
    def check(): Boolean

    def getType(): Type
  }

  case class ArrayLiter(elems: List[Expr]) extends RValue {
    def check(): Boolean = {
      var valid: Boolean = true
      for (elem <- elems) {
        valid = valid && elem.check()
      }
      var t1: Type = elems.head.getType()
      for (elem <- elems) {
        def tn: Type = elem.getType()
        // MUST CONSIDER [char[], string] CASE
        if (tn != t1) {
          return false
        }
      }
      return valid
    }

    def getType(): Type = {
      return ArrayT(elems.head.getType(), 1)
    }
  }

  case class NewPair(exp1: Expr, exp2: Expr) extends RValue {
    def check(): Boolean = {
      if (!exp1.check() || !exp2.check()) {
        return false
      }
      if (exp1.getType() != BaseT("int") || exp2.getType() != BaseT("int")) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return PairT(exp1.getType().asInstanceOf[PairElemT], exp2.getType().asInstanceOf[PairElemT])
    }
  }

  case class PairElem(func: String, lvalue: LValue) extends RValue with LValue {
    def check(): Boolean = {
      return lvalue.check()
    }

    def getType(): Type = {
      def parentT: PairT = lvalue.getType().asInstanceOf[PairT]

      def childT: PairElemT = func match {
        case "fst" => parentT.pet1
        case "snd" => parentT.pet2
      }

      return childT
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

    def getType(): Type = {
      // GET TYPE OF FUNC FROM SYMBOL TABLE
      return BaseT("ERROR")
    }
  }

  sealed trait Expr extends RValue {
    def check(): Boolean

    def getType(): Type
  }

  case class Mul(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp1.check() || !exp2.check()) {
        return false
      }
      if (exp1.getType() != BaseT("int") || exp2.getType() != BaseT("int")) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("int")
    }
  }

  case class Div(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp1.check() || !exp2.check()) {
        return false
      }
      if (exp1.getType() != BaseT("int") || exp2.getType() != BaseT("int")) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("int")
    }
  }

  case class Mod(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp1.check() || !exp2.check()) {
        return false
      }
      if (exp1.getType() != BaseT("int") || exp2.getType() != BaseT("int")) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("int")
    }
  }

  case class Add(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp1.check() || !exp2.check()) {
        return false
      }
      if (exp1.getType() != BaseT("int") || exp2.getType() != BaseT("int")) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("int")
    }
  }

  case class Sub(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp1.check() || !exp2.check()) {
        return false
      }
      if (exp1.getType() != BaseT("int") || exp2.getType() != BaseT("int")) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("int")
    }
  }

  case class GT(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp1.check() || !exp2.check()) {
        return false
      }

      def type1: Type = exp1.getType()

      def type2: Type = exp2.getType()

      if ((type1 != BaseT("int") && type1 != BaseT("char")) ||
        (type2 != BaseT("int") && type2 != BaseT("char"))) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("bool")
    }
  }

  case class GTEQ(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp1.check() || !exp2.check()) {
        return false
      }

      def type1: Type = exp1.getType()

      def type2: Type = exp2.getType()

      if ((type1 != BaseT("int") && type1 != BaseT("char")) ||
        (type2 != BaseT("int") && type2 != BaseT("char"))) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("bool")
    }
  }

  case class LT(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp1.check() || !exp2.check()) {
        return false
      }

      def type1: Type = exp1.getType()

      def type2: Type = exp2.getType()

      if ((type1 != BaseT("int") && type1 != BaseT("char")) ||
        (type2 != BaseT("int") && type2 != BaseT("char"))) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("bool")
    }
  }

  case class LTEQ(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp1.check() || !exp2.check()) {
        return false
      }

      def type1: Type = exp1.getType()

      def type2: Type = exp2.getType()

      if ((type1 != BaseT("int") && type1 != BaseT("char")) ||
        (type2 != BaseT("int") && type2 != BaseT("char"))) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("bool")
    }
  }

  case class EQ(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp1.check() || !exp2.check()) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("bool")
    }
  }

  case class NEQ(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp1.check() || !exp2.check()) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("bool")
    }
  }

  case class And(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp1.check() || !exp2.check()) {
        return false
      }
      if (exp1.getType() != BaseT("bool") || exp2.getType() != BaseT("bool")) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("bool")
    }
  }

  case class Or(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp1.check() || !exp2.check()) {
        return false
      }
      if (exp1.getType() != BaseT("bool") || exp2.getType() != BaseT("bool")) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("bool")
    }
  }

  case class Not(exp: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp.check() || !(exp.getType() == BaseT("bool"))) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("bool")
    }
  }

  case class Neg(exp: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp.check() || !(exp.getType() == BaseT("int"))) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("int")
    }
  }

  case class Len(exp: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp.check()) {
        return false
      }

      def arrT: Type = exp.getType()

      def valid = arrT match {
        case ArrayT(_, n) if n > 0 => true
        case _ => false
      }

      return valid
    }

    def getType(): Type = {
      return BaseT("int")
    }
  }

  case class Ord(exp: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp.check() || !(exp.getType() == BaseT("char"))) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("int")
    }
  }

  case class Chr(exp: Expr) extends Expr {
    def check(): Boolean = {
      if (!exp.check() || !(exp.getType() == BaseT("int"))) {
        return false
      }
      return true
    }

    def getType(): Type = {
      return BaseT("char")
    }
  }

  sealed trait Atom extends Expr

  case class Num(value: Int) extends Atom {
    def check(): Boolean = {
      return true
    }

    def getType(): Type = {
      return BaseT("int")
    }
  }

  case class Bool(bool: String) extends Atom {
    def check(): Boolean = {
      return bool == "true" || bool == "false"
    }

    def getType(): Type = {
      return BaseT("bool")
    }
  }

  case class Ch(chr: Char) extends Atom {
    def check(): Boolean = {
      return true
    }

    def getType(): Type = {
      return BaseT("char")
    }
  }

  case class Str(str: String) extends Atom {
    def check(): Boolean = {
      return true
    }

    def getType(): Type = {
      return BaseT("string")
    }
  }

  case class PairLiter(str: String) extends Atom with Type {
    def check(): Boolean = {
      return true;
    }

    def getType(): Type = {
      return PairLiter(str)
    }
  }

  case class Ident(str: String) extends Atom with LValue {
    def check(): Boolean = {
      // Need symbol table code
      return false
    }

    def getType(): Type = {
      // MUST FIND FROM SYMBOL TABLE
      return BaseT("ERROR")
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

    def getType(): Type = {
      return ident.getType()
    }
  }
}
