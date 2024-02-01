package wacc

import parsley.Parsley.{atomic, many, some}
import parsley.character.string
import parsley.combinator.{option, sepBy}
import parsley.expr._
import parsley.{Parsley, Result}
import wacc.ASTNodes._
import wacc.lexer._
import wacc.lexer.implicits.implicitSymbol

object parser {
    def parse(input: String): Result[String, Prog] = parser.parse(input)

    private val parser = fully(rvalue)

    private lazy val prog = "begin" ~> many(func) <~> statement <~ "end"

    private lazy val statement: Parsley[Statement] = {
        atomic(declare) |
        atomic(lvalue <~ "=" ~> rvalue) |
            atomic("read" ~> lvalue) |
            atomic("free" ~> expr) |
            atomic("return" ~> expr) |
            atomic("exit" ~> expr) |
            atomic("print" ~> expr) |
            atomic("println" ~> expr) |
            atomic("if" ~> expr <~ "then" ~> statement <~ "else" ~> statement <~ "fi") |
            atomic("while" ~> expr <~ "do" ~> statement <~ "done") |
            atomic("begin" ~> statement <~ "end") |
            atomic("skip" ~> statement) |
            atomic(statement <~ ";" <~ statement)
    }

    private lazy val func  = types <~> ident <~ "(" <~> option(paramList) <~ ")" <~ "is" <~ statement <~ "end"

    private lazy val paramList = param <+:> many("," ~> param)

    private lazy val param = types <~> ident

    private lazy val arrayElem = atomic(ident <~> some("[" ~> expr <~ "]"))

    private lazy val pairElem = string("fst") <~> lvalue | string("snd") <~> lvalue

    private lazy val lvalue: Parsley[Statement] = {
        atomic(ident.map(Var)) | atomic(arrayElem).map(x => ArrayElem(x._1, x._2)) |
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
        atomic((types <~> ident <~ "=") <~> rvalue).map(x => Declare(x._1._1, x._1._2, x._2))
    }

    private lazy val types: Parsley[String] = {
        ((baseType | pairType) <~> many("[]")).map{case (base: String, arr: List[Unit]) => base + arr.mkString}
    }

    private lazy val pairType: Parsley[String] = atomic("pair" ~> "(" ~> pairElemType <~ "," <~ pairElemType <~ ")")

    private lazy val pairElemType: Parsley[String] = atomic(baseType) | atomic(string("pair"))

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
