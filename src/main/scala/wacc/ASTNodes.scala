package wacc

import wacc.Main.{SEMANTIC_ERR_CODE, SUCCESS_CODE}

object ASTNodes {

  var currentSymbolTable: SymbolTable = new SymbolTable(None)

  def checkValid(valid: Boolean, str: String, node: ASTNode): Unit = {
    if (!valid) {
      println(s"Invalid $str")
      println(node)
      System.exit(SEMANTIC_ERR_CODE)
    }
  }

  sealed trait ASTNode
  case class Program(funcs: List[Function], statement: Statement) extends ASTNode {
    val symbolTable: SymbolTable = new SymbolTable(None)

    def check(): Unit = {
      var valid: Boolean = true
      currentSymbolTable = symbolTable
      for (func <- funcs) {
        valid = valid && func.check()
        checkValid(valid, "function", func)
      }
      statement match {
        case statements: Statements =>
          for (stat <- statements.stmts) {
            stat match {
              case Return(_) => System.exit(SEMANTIC_ERR_CODE)
              case _ =>
            }
          }
        case _ =>
      }
      if (valid && statement.check()) {
        System.exit(SUCCESS_CODE)
      } else {
        System.exit(SEMANTIC_ERR_CODE)
      }
    }
  }

  case class Function(_type: Type, ident: Ident, param_list: List[Param], body: Statement) extends ASTNode {
    val symbolTable: SymbolTable = new SymbolTable(None)

    def check(): Boolean = {
      var valid: Boolean = true
      val tempSymbolTable: SymbolTable = currentSymbolTable
      currentSymbolTable = symbolTable
      valid = valid && body.check()
      checkValid(valid, "body", body)
      currentSymbolTable = tempSymbolTable
      valid
    }
  }

  case class Param(_type: Type, ident: Ident) extends ASTNode {
    def check(): Boolean = {
      _type == ident.getType()
    }

    def getType(): Type = {
      _type.getType()
    }
  }

  sealed trait Statement extends ASTNode {
    def check(): Boolean
  }

  case class Skip() extends Statement {
    def check(): Boolean = {
      true
    }
  }

  case class Declare(_type: Type, ident: Ident, value: RValue) extends Statement {
    def check(): Boolean = {
      var valid: Boolean = value.check()
      checkValid(valid, "Rvalue", value)
      valid = valid && _type == value.getType()
      checkValid(valid, "type not same as Rvalue type", ident)
      valid
    }
  }

  case class Assign(lvalue: LValue, rvalue: RValue) extends Statement {
    def check(): Boolean = {
      var valid: Boolean = lvalue.check() && rvalue.check()
      checkValid(valid, "lvalue and rvalue", lvalue)
      valid = valid && lvalue.getType() == rvalue.getType()
      checkValid(valid, "lvalue and rvalue type", lvalue)
      valid
    }
  }

  case class Read(lvalue: LValue) extends Statement {
    def check(): Boolean = {
      lvalue.check()
    }
  }

  case class If(cond: Expr, thenS: Statement, elseS: Statement) extends Statement {
    val thenSymbolTable: SymbolTable = new SymbolTable(None)
    val elseSymbolTable: SymbolTable = new SymbolTable(None)

    def check(): Boolean = {
      if (!cond.check() || !thenS.check() || !elseS.check()) {
        false
      } else if (cond.getType() != BaseT("bool")) {
        false
      } else {
        val tempSymbolTable = currentSymbolTable
        currentSymbolTable = thenSymbolTable
        var valid: Boolean = true
        valid = thenS.check()
        checkValid(valid, "then statement", thenS)

        currentSymbolTable = elseSymbolTable
        valid = elseS.check()
        checkValid(valid, "else statement", elseS)

        currentSymbolTable = tempSymbolTable
        valid
      }
    }
  }

  case class While(cond: Expr, body: Statement) extends Statement {
    val symbolTable: SymbolTable = new SymbolTable(None)

    def check(): Boolean = {
      val tempSymbolTable: SymbolTable = currentSymbolTable
      currentSymbolTable = symbolTable
      val valid = cond.check() && body.check() && cond.getType() == BaseT("bool")
      checkValid(valid, "while statement", body)
      currentSymbolTable = tempSymbolTable
      valid
    }
  }

  case class Scope(body: Statement) extends Statement {
    val symbolTable: SymbolTable = new SymbolTable(None)

    def check(): Boolean = {
      val tempSymbolTable: SymbolTable = currentSymbolTable
      currentSymbolTable = symbolTable
      body.check()
      currentSymbolTable = tempSymbolTable
      true
    }
  }

  case class Statements(stmts: List[Statement]) extends Statement {
    def check(): Boolean = {
      var valid: Boolean = true
      for (stat <- stmts) {
        valid = valid && stat.check()
        checkValid(valid, "statement", stat)
      }
      valid
    }
  }

  case class Free(exp: Expr) extends Statement {
    def check(): Boolean = {
      val t: Type = exp.getType()

      exp.check() && (t match {
        case ArrayT(_, n) if n > 0 => true
        case PairT(_, _) => true
        case _ => false
      })
    }
  }

  case class Return(exp: Expr) extends Statement {
    def check(): Boolean = {
      exp.check()
    }
  }

  case class Exit(exp: Expr) extends Statement {
    def check(): Boolean = {
      exp.check() && exp.getType() == BaseT("int")
    }
  }

  case class Print(exp: Expr) extends Statement {
    def check(): Boolean = {
      exp.check()
    }
  }
  case class Println(exp: Expr) extends Statement {
    def check(): Boolean = {
      exp.check()
    }
  }


  sealed trait Type extends ASTNode {
    def check(): Boolean

    def getType(): Type
  }

  sealed trait PairElemT extends Type {
    def check(): Boolean

    def getType(): Type
  }

  case class BaseT(str: String) extends Type with PairElemT {
    def check(): Boolean = {
      str == "int" || str == "bool" || str == "char" || str == "string"
    }

    def getType(): Type = {
      BaseT(str)
    }
  }

  case class ArrayT(_type: Type, dim: Int) extends Type with PairElemT {
    def check(): Boolean = {
      _type.check()
    }

    def getType(): Type = {
      ArrayT(_type, dim)
    }
  }

  case class PairT(pet1: PairElemT, pet2: PairElemT) extends Type {
    def check(): Boolean = {
      pet1.check() && pet2.check()
    }

    def getType(): Type = {
      PairT(pet1, pet2)
    }
  }

  case class PairNull() extends PairElemT {
    def check(): Boolean = {
      true
    }

    def getType(): Type = {
      PairNull()
    }
  }

  sealed trait LValue extends ASTNode {
    def check(): Boolean

    def getType(): Type
  }

  sealed trait RValue extends ASTNode {
    def check(): Boolean

    def getType(): Type
  }

  case class ArrayLiter(elems: List[Expr]) extends RValue {
    def check(): Boolean = {
      var valid: Boolean = true
      for (elem <- elems) {
        valid = valid && elem.check()
        checkValid(valid, "array elem", elem)
      }
      if (elems.isEmpty) {
        valid = true
      } else {
        var t1: Type = elems.head.getType()
        for (elem <- elems) {
          def tn: Type = elem.getType()
          // MUST CONSIDER [char[], string] CASE
          if (tn != t1) {
            false
          }
        }
      }
      valid
    }

    def getType(): Type = {
      if (elems.isEmpty) {
        /* Need to fix type */
        ArrayT(BaseT("ERROR"), 0)
      } else {
        ArrayT(elems.head.getType(), 1)
      }
    }
  }

  case class NewPair(exp1: Expr, exp2: Expr) extends RValue {
    def check(): Boolean = {
      exp1.check() && exp2.check() && exp1.getType() == BaseT("int") && exp2.getType() == BaseT("int")
    }

    def getType(): Type = {
      PairT(exp1.getType().asInstanceOf[PairElemT], exp2.getType().asInstanceOf[PairElemT])
    }
  }

  case class PairElem(func: String, lvalue: LValue) extends RValue with LValue {
    def check(): Boolean = {
      lvalue.check()
    }

    def getType(): Type = {
      def parentT: PairT = lvalue.getType().asInstanceOf[PairT]

      def childT: PairElemT = func match {
        case "fst" => parentT.pet1
        case "snd" => parentT.pet2
      }

      childT
    }
  }

  case class Call(funcName: Ident, args: List[Expr]) extends RValue {
    def check(): Boolean = {
      var valid: Boolean = true
      valid = valid && funcName.check()
      checkValid(valid, "function name", funcName)
      for (arg <- args) {
        valid = valid && arg.check()
        checkValid(valid, "argument", arg)
      }
      valid
    }

    def getType(): Type = {
      funcName.getType()
    }
  }

  sealed trait Expr extends RValue {
    def check(): Boolean

    def getType(): Type
  }

  case class Mul(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      exp1.check() && exp2.check() && exp1.getType() == BaseT("int") && exp2.getType() == BaseT("int")
    }

    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Div(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      exp1.check() && exp2.check() && exp1.getType() == BaseT("int") && exp2.getType() == BaseT("int")
    }

    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Mod(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      exp1.check() && exp2.check() && exp1.getType() == BaseT("int") && exp2.getType() == BaseT("int")
    }

    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Add(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      exp1.check() && exp2.check() && exp1.getType() == BaseT("int") && exp2.getType() == BaseT("int")
    }

    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Sub(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      exp1.check() && exp2.check() && exp1.getType() == BaseT("int") && exp2.getType() == BaseT("int")
    }

    def getType(): Type = {
      BaseT("int")
    }
  }

  case class GT(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      val type1: Type = exp1.getType()
      val type2: Type = exp2.getType()

      exp1.check() && exp2.check() &&
        ((type1 == BaseT("int") && type2 == BaseT("int")) ||
          (type1 == BaseT("char") && type2 == BaseT("char")))
    }

    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class GTEQ(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      val type1: Type = exp1.getType()
      val type2: Type = exp2.getType()

      exp1.check() && exp2.check() &&
        ((type1 == BaseT("int") && type2 == BaseT("int")) ||
          (type1 == BaseT("char") && type2 == BaseT("char")))
    }

    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class LT(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      val type1: Type = exp1.getType()
      val type2: Type = exp2.getType()

      exp1.check() && exp2.check() &&
        ((type1 == BaseT("int") && type2 == BaseT("int")) ||
          (type1 == BaseT("char") && type2 == BaseT("char")))
    }

    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class LTEQ(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      val type1: Type = exp1.getType()
      val type2: Type = exp2.getType()

      exp1.check() && exp2.check() &&
        ((type1 == BaseT("int") && type2 == BaseT("int")) ||
          (type1 == BaseT("char") && type2 == BaseT("char")))
    }

    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class EQ(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      exp1.check() && exp2.check()
    }

    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class NEQ(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      exp1.check() && exp2.check()
    }

    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class And(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      exp1.check() && exp2.check() && exp1.getType() == BaseT("bool") && exp2.getType() == BaseT("bool")
    }

    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class Or(exp1: Expr, exp2: Expr) extends Expr {
    def check(): Boolean = {
      exp1.check() && exp2.check() && exp1.getType() == BaseT("bool") && exp2.getType() == BaseT("bool")
    }

    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class Not(exp: Expr) extends Expr {
    def check(): Boolean = {
      exp.check() && exp.getType() == BaseT("bool")
    }

    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class Neg(exp: Expr) extends Expr {
    def check(): Boolean = {
      exp.check() && exp.getType() == BaseT("int")
    }

    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Len(exp: Expr) extends Expr {
    def check(): Boolean = {
      val arrT: Type = exp.getType()

      exp.check() && (arrT match {
        case ArrayT(_, n) if n > 0 => true
        case _ => false
      })
    }

    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Ord(exp: Expr) extends Expr {
    def check(): Boolean = {
      exp.check() && exp.getType() == BaseT("char")
    }

    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Chr(exp: Expr) extends Expr {
    def check(): Boolean = {
      exp.check() && exp.getType() == BaseT("int")
    }

    def getType(): Type = {
      BaseT("char")
    }
  }

  sealed trait Atom extends Expr

  case class Num(value: Int) extends Atom {
    def check(): Boolean = {
      true
    }

    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Bool(bool: String) extends Atom {
    def check(): Boolean = {
      bool == "true" || bool == "false"
    }

    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class Ch(chr: Char) extends Atom {
    def check(): Boolean = {
      true
    }

    def getType(): Type = {
      BaseT("char")
    }
  }

  case class Str(str: String) extends Atom {
    def check(): Boolean = {
      true
    }

    def getType(): Type = {
      BaseT("string")
    }
  }

  case class PairLiter(str: String) extends Atom with Type {
    def check(): Boolean = {
      true
    }

    def getType(): Type = {
      PairLiter(str)
    }
  }

  case class Ident(str: String) extends Atom with LValue {
    def check(): Boolean = {
      currentSymbolTable.lookupAll(str).isDefined
    }

    def getType(): Type = {
      currentSymbolTable.lookupAll(str) match {
        case Some(x) => x match {
          case param: Param =>
            param.getType()
          case declare: Declare =>
            declare._type
          case function: Function =>
            function._type
          case _ =>
            BaseT("ERROR")
        }
        case None => BaseT("ERROR")
      }
    }
  }

  case class ArrayElem(ident: Ident, args: List[Expr]) extends Atom with LValue {
    def check(): Boolean = {
      var valid: Boolean = true
      valid = valid && ident.check()
      for (arg <- args) {
        valid = valid && (arg.getType() == ident.getType())
        checkValid(valid, "array elem", arg)
      }
      valid
    }

    def getType(): Type = {
      ident.getType()
    }
  }
}
