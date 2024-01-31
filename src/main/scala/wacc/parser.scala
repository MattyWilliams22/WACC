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

    private val parser = fully(statement)

    sealed trait Prog

    sealed trait Statement extends Prog
    case class NewPair(x: Expr) extends Statement
    case class PairElem(x: String, y: Statement) extends Statement
    case class Call(x: String, y: List[Expr]) extends Statement

    sealed trait Expr extends Prog with Statement
    case class Mul(x: Expr, y: Expr) extends Expr
    case class Div(x: Expr, y: Expr) extends Expr
    case class Mod(x: Expr, y: Expr) extends Expr
    case class Add(x: Expr, y: Expr) extends Expr
    case class Sub(x: Expr, y: Expr) extends Expr
    case class GT(x: Expr, y: Expr) extends Expr
    case class GTEQ(x: Expr, y: Expr) extends Expr
    case class LT(x: Expr, y: Expr) extends Expr
    case class LTEQ(x: Expr, y: Expr) extends Expr
    case class EQ(x: Expr, y: Expr) extends Expr
    case class NEQ(x: Expr, y: Expr) extends Expr
    case class And(x: Expr, y: Expr) extends Expr
    case class Or(x: Expr, y: Expr) extends Expr
    case class Not(x: Expr) extends Expr
    case class Neg(x: Expr) extends Expr
    case class Len(x: Expr) extends Expr
    case class Ord(x: Expr) extends Expr
    case class Chr(x: Expr) extends Expr
    case class Num(x: BigInt) extends Expr

    sealed trait Atom extends Expr
    case class Var(x: String) extends Atom with Statement
    case class Bool(x: String) extends Atom
    case class Ch(x: Char) extends Atom
    case class Str(x: String) extends Atom
    case class PairLiter(x: String) extends Atom
    case class ArrayElem(x: String, y: List[Expr]) extends Atom with Statement

    private lazy val statement: Parsley[Statement] = {rvalue// statement <~ ";" ~> statement
//            ("skip" ^^^ Skip) |
//            ("read" ~> lvalue) |
//            ("free" ~> lvalue) |
//            ("return" ~> expr) |
//            ("exit" ~> expr) |
//            ("print" ~> expr) |
//            ("println" ~> expr) |
//            ("if" ~> expr <~ "then") ~ statement ~ ("else" ~> statement <~ "fi") |
//            ("while" ~> expr <~ "do") ~ statement ~ ("done" ~> expr) |
//            ("begin" ~> statement <~ "end") |
//            ("newpair" ~> expr <~ "," <~ expr) |
//            ("call" ~> ident <~ "(" ~> option(expr <~> many("," ~> expr)) <~ ")")

    }

    private lazy val arrayElem = ident <~> some("[" ~> expr <~ "]")

    private lazy val pairElem = string("fst") <~> lvalue | string("snd") <~> lvalue

    private lazy val lvalue: Parsley[Statement] = {
        ident.map(Var) | atomic(arrayElem).map(x => ArrayElem(x._1, x._2)) |
            atomic(pairElem).map(x => PairElem(x._1, x._2))
    }

    private lazy val rvalue: Parsley[Statement] = {
        expr | ("newpair(" ~> expr <~ "," ~> expr <~ ")").map(NewPair)
//            (("call" ~> ident <~ "(") <~> option(sepBy(expr, ",")) <~ ")").map {
//                case Success((name: String, Some(x: List[Expr]))) => Call(name, x)
//            }
//        ("[" ~> option(expr <~> many("," <~ expr)) <~ "]")

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
