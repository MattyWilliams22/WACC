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
  private [wacc] def parseTest(input: String): Result[String, Expr] = parserTest.parse(input)

  private lazy val parserTest = fully(expr)
  private val parser = fully(prog)

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

  private lazy val arrayElem = ident <~> some("[" ~> expr <~ "]")

  private lazy val types: Parsley[Type] = {
    (pairType <~> many("[]")).map {
      case (base: PairT, arr: List[Unit]) if arr.isEmpty => base
      case (base: PairT, _: List[Unit]) => ArrayT(base)
    } |
      (baseTypes <~> many("[]")).map {
        case (base: BaseT, arr: List[Unit]) if arr.isEmpty => base
        case (base: BaseT, _: List[Unit]) => ArrayT(base)
      }
  }

  private lazy val baseTypes: Parsley[BaseT] = {
    baseType.map(BaseT)
  }

  private lazy val pairType: Parsley[PairT] =
    ("pair(" ~> pairElemType <~ "," <~> pairElemType <~ ")").map(x => PairT(x._1, x._2))

  private lazy val pairElemType: Parsley[PairElemT] =
    atomic(pairType <~> some("[]")).map {
      case (base: PairT, _: List[Unit]) => ArrayT(base)
    } |
      atomic("pair").map(_ => PairNull()) |
      (baseTypes <~> many("[]")).map {
        case (base: BaseT, arr: List[Unit]) if arr.isEmpty => base
        case (base: BaseT, _: List[Unit]) => ArrayT(base)
      }

  private lazy val prog: Parsley[Prog] =
    ("begin" ~> many(atomic(func)) <~> stmt <~ "end").map(x => Prog(x._1, x._2))

  private lazy val func: Parsley[Func] =
    ((((types <~> ident <~ "(") <~> option(paramList) <~ ")") <~ "is") <~> stmt <~ "end").map {
      case (((t, i), Some(ps)), s) => Func(t, Ident(i), ps, s)
      case (((t, i), None), s) => Func(t, Ident(i), List(), s)
  }

  private lazy val paramList: Parsley[List[Param]] =
    (param <~> many("," ~> param)).map(x => x._1 :: x._2)

  private lazy val param: Parsley[Param] = (types <~> ident).map(x => Param(x._1, Ident(x._2)))

  private lazy val stmt: Parsley[Stmt] = {
    ((atomic("skip").map(_ => Skip()) |
      atomic(declare) |
      (lvalue <~> ("=" ~> rvalue)).map(x => Assign(x._1, x._2)) |
      (read ~> lvalue).map(Read) |
      (free <~> expr).map(x => Action(x._1, x._2)) |
      (ret <~> expr).map(x => Action(x._1, x._2)) |
      (exit <~> expr).map(x => Action(x._1, x._2)) |
      atomic(println <~> expr).map(x => Action(x._1, x._2)) |
      atomic(print <~> expr).map(x => Action(x._1, x._2)) |
      ifElse |
      (("while" ~> expr <~ "do") <~> stmt <~ "done").map(x => While(x._1, x._2)) |
      ("begin" ~> stmt <~ "end").map(Scope)) <~> many(";" ~> stmt)).map(x => Stmts(x._1 :: x._2))
  }

  private lazy val declare: Parsley[Declare] = {
    ((types <~> ident) <~> ("=" ~> rvalue)).map {
      case ((t, i), r) => Declare(t, Ident(i), r)
    }
  }

  private lazy val ifElse: Parsley[If] = {
    ((("if" ~> expr <~ "then") <~> stmt <~ "else") <~> stmt <~ "fi").map {
      case ((cond, thenS), elseS) => If(cond, thenS, elseS)
    }
  }

  private lazy val pairElem: Parsley[(String, LValue)] = fst <~> lvalue | snd <~> lvalue

  private lazy val lvalue: Parsley[LValue] = {
    ident.map(Ident) | atomic(arrayElem).map(x => ArrayElem(Ident(x._1), x._2)) |
      atomic(pairElem).map(x => PairElem(x._1, x._2))
  }

  private lazy val rvalue: Parsley[RValue] = {
    atomic("newpair(" ~> expr <~ "," <~> expr <~ ")").map(x => NewPair(x._1, x._2)) |
      (("call" ~> ident <~ "(") <~> option(sepBy(expr, ",")) <~ ")").map {
        case (name: String, Some(x: List[Expr])) => Call(Ident(name), x)
        case (name: String, None) => Call(Ident(name), List())
      } |
      atomic(expr) |
      atomic(pairElem).map(x => PairElem(x._1, x._2)) |
      ("[" ~> option(expr <+:> many("," ~> expr)) <~ "]").map {
        case Some(x: List[Expr]) => ArrayLiter(x)
        case None => ArrayLiter(List())
      }
  }

  private lazy val atom: Parsley[Expr] = {
    "(" ~> expr <~ ")" | atomic(arrayElem).map(x => ArrayElem(Ident(x._1), x._2)) |
      int.map(Num) | ident.map(Ident) | bool.map(Bool) |
      char.map(Ch) | str.map(Str) | pairLiter.map(PairLiter)
  }
}
