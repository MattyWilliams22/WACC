package wacc

import parsley.Parsley.{atomic, many, some}
import parsley.character.string
import parsley.combinator.{option, sepBy}
import parsley.expr._
import parsley.{Parsley, Result}
import wacc.lexer.implicits.implicitSymbol
import wacc.lexer._
import wacc.ASTNodes._

object parser {
    def parse(input: String): Result[String, Prog] = parser.parse(input)

    private val parser = fully(rvalue)

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
            ("skip" ~> statement) |
            declare
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

    private lazy val declare = {
        (types <~> ident <~ "=" ~> rvalue).map(x => Declare(x._1, x._2))
    }

    private lazy val types: Parsley[String] = {
        baseType | arrayType | pairType
    }

    private lazy val baseType: Parsley[String] = {
        string("int") | string("bool") | string("char") | string("string") | string("pair")
    }

    private lazy val arrayType: Parsley[String] = types <~ "[]"

    private lazy val pairType: Parsley[String] = "pair" ~> "(" ~> pairElemType <~ "," <~ pairElemType <~ ")"

    private lazy val pairElemType: Parsley[String] = baseType | arrayType | string("pair")

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
