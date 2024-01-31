package wacc

import parsley.Parsley.{atomic, many, some}
import parsley.character.string
import parsley.combinator.{option, sepBy}
import parsley.expr._
import parsley.{Parsley, Result, Success}
import wacc.lexer.implicits.implicitSymbol
import wacc.lexer._

object parser {
    def parse(input: String): Result[String, Prog] = parser.parse(input)

    private val parser = fully(rvalue)

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
    case class Num(value: BigInt) extends Expr

    sealed trait Atom extends Expr
    case class Var(varName: String) extends Atom with Statement
    case class Bool(bool: String) extends Atom
    case class Ch(chr: Char) extends Atom
    case class Str(str: String) extends Atom
    case class PairLiter(str: String) extends Atom
    case class ArrayElem(varName: String, args: List[Expr]) extends Atom with Statement

    private lazy val statement: Parsley[Statement] = {
            (lvalue <~ "=" ~> rvalue) |
            ("read" ~> lvalue) |
            ("free" ~> expr) |
            ("return" ~> expr) |
            ("exit" ~> expr) |
            ("print" ~> expr) |
            ("println" ~> expr) |
            ("if" ~> expr <~ "then" ~> statement <~ "else" ~> statement <~ "fi") |
            ("while" ~> expr <~ "do" ~> statement <~ "done") |
            ("begin" ~> statement <~ "end") |
            (statement <~ ";" <~ statement) |
            ("skip" ~> statement)
    }

    private lazy val arrayElem = ident <~> some("[" ~> expr <~ "]")

    private lazy val pairElem = string("fst") <~> lvalue | string("snd") <~> lvalue

    private lazy val lvalue: Parsley[Statement] = {
        ident.map(Var) | atomic(arrayElem).map(x => ArrayElem(x._1, x._2)) |
            atomic(pairElem).map(x => PairElem(x._1, x._2))
    }

    private lazy val rvalue: Parsley[Statement] = {
        atomic("newpair(" ~> expr <~ "," <~> expr <~ ")").map(x => NewPair(x._1, x._2)) |
          (("call" ~> ident <~ "(") <~> option(sepBy(expr, ",")) <~ ")").map {
              case (name: String, Some(x: List[Expr])) => Call(name, x)
              case (name: String, None) => Call(name, List())
          } |
          atomic(expr) |
          atomic(pairElem).map(x => PairElem(x._1, x._2)) |
          ("[" ~> option(expr <+:> many("," ~> expr)) <~ "]").map {
              case Some(x: List[Expr]) => ArrayLiter(x)
              case None => ArrayLiter(List())
          }
    }

    private lazy val atom: Parsley[Expr] = {
        "(" ~> expr <~ ")" | atomic(arrayElem).map(x => ArrayElem(x._1, x._2)) |
            int.map(Num) | ident.map(Var) | bool.map(Bool) | char.map(Ch) | str.map(Str) |
            pairLiter.map(PairLiter)
    }

    private lazy val expr: Parsley[Expr] = {
        precedence(atom)(
            Ops(Prefix)("!" as Not),
            Ops(Prefix)("-" as Neg),
            Ops(Prefix)("len" as Len),
            Ops(Prefix)("ord" as Ord),
            Ops(Prefix)("chr" as Chr),
            Ops(InfixL)("*" as Mul),
            Ops(InfixL)("%" as Mod),
            Ops(InfixL)("/" as Div),
            Ops(InfixL)("+" as Add),
            Ops(InfixL)("-" as Sub),
            Ops(InfixN)(">" as GT),
            Ops(InfixN)(">=" as GTEQ),
            Ops(InfixN)("<" as LT),
            Ops(InfixN)("<=" as LTEQ),
            Ops(InfixN)("==" as EQ),
            Ops(InfixN)("!=" as NEQ),
            Ops(InfixR)("&&" as And),
            Ops(InfixR)("||" as Or)
        )
    }
}
