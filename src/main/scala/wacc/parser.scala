package wacc

import parsley.Parsley.{atomic, many, some}
import parsley.combinator.{option, sepBy}
import parsley.expr._
import parsley.{Parsley, Result}
import parsley.character.whitespace
import wacc.ASTNodes._
import wacc.lexer._
import wacc.lexer.implicits.implicitSymbol

object parser {
  def parse(input: String): Result[String, Program] = parser.parse(input)
  private [wacc] def parseTest(input: String): Result[String, Expr] = parserTest.parse(input)

  private lazy val parserTest = fully(expr)
  private val parser = fully(prog)

  private lazy val prog: Parsley[Program] =
    (begin ~> many(atomic(func)) <~> statement <~ end).map(x => Program(x._1, x._2))

  private lazy val func: Parsley[Function] =
    (((types <~> ident <~ "(" <~> option(paramList) <~ ")") <~ is) <~> statement.filter(bodyHasReturnOrExit) <~ end).map {
      case (((t, i), Some(ps)), s) => Function(t, Ident(i), ps, s)
      case (((t, i), None), s) => Function(t, Ident(i), List(), s)
    }

  private def bodyHasReturnOrExit(s: Statement): Boolean = s match {
    case Statements(x) => bodyHasReturnOrExit(x.last)
    case If(_, thenS, elseS) => bodyHasReturnOrExit(thenS) && bodyHasReturnOrExit(elseS)
    case While(_, body) => bodyHasReturnOrExit(body)
    case Scope(body) => bodyHasReturnOrExit(body)
    case Action("return", _) | Action("exit", _) => true
    case _ => false
  }

  private lazy val paramList: Parsley[List[Param]] =
    (param <~> many("," ~> param)).map(x => x._1 :: x._2)

  private lazy val param: Parsley[Param] = (types <~> ident).map(x => Param(x._1, Ident(x._2)))

  private lazy val statement: Parsley[Statement] = {
    ((atomic(skip).map(_ => Skip()) |
      atomic(read ~> lvalue).map(Read) |
      atomic(free <~> expr).map(x => Action(x._1, x._2)) |
      atomic(ret <~> expr).map(x => Action(x._1, x._2)) |
      atomic(exit <~> expr).map(x => Action(x._1, x._2)) |
      atomic(println <~> expr).map(x => Action(x._1, x._2)) |
      atomic(print <~> expr).map(x => Action(x._1, x._2)) |
      atomic(ifElse) |
      atomic(declare) |
      atomic(lvalue <~> ("=" ~> rvalue)).map(x => Assign(x._1, x._2)) |
      atomic((WHILE ~> expr <~ DO) <~> statement <~ done).map(x => While(x._1, x._2)) |
      atomic(begin ~> statement <~ end).map(Scope)) <~> many(";" ~> statement)).map(x => Statements(x._1 :: x._2))
  }

  private lazy val lvalue: Parsley[LValue] = {
    atomic(arrayElem).map(x => ArrayElem(Ident(x._1), x._2)) |
      atomic(pairElem).map(x => PairElem(x._1, x._2)) | atomic(ident).map(Ident)
  }

  private lazy val rvalue: Parsley[RValue] = {
    atomic("newpair(" ~> expr <~ "," <~> expr <~ ")").map(x => NewPair(x._1, x._2)) |
      atomic((call ~> ident <~ "(") <~> option(sepBy(expr, ",")) <~ ")").map {
        case (name: String, Some(x: List[Expr])) => Call(Ident(name), x)
        case (name: String, None) => Call(Ident(name), List())
      } |
      atomic(expr) |
      atomic(pairElem).map(x => PairElem(x._1, x._2)) |
      ("[" ~> option(expr <+:> many("," ~> expr)) <~ "]").map {
        case Some(x: Seq[Expr]) => ArrayLiter(x.toList)
        case None => ArrayLiter(List())
      }
  }

  private lazy val arrayElem = ident <~> some("[" ~> expr <~ "]")

  private lazy val pairElem: Parsley[(String, LValue)] = fst <~> lvalue | snd <~> lvalue

  private lazy val ifElse: Parsley[If] = {
    (((IF ~> expr <~ THEN) <~> statement <~ ELSE) <~> statement <~ fi).map {
      case ((cond, thenS), elseS) => If(cond, thenS, elseS)
    }
  }

  private lazy val declare: Parsley[Declare] = {
    ((types <~> ident) <~> ("=" ~> rvalue)).map {
      case ((t, i), r) => Declare(t, Ident(i), r)
    }
  }

  private lazy val types: Parsley[Type] = {
    (pairType <~> many(arrayBraces) <~ some(whitespace)).map {
      case (base: PairT, arr: List[String]) if arr.isEmpty => base
      case (base: PairT, arr: List[String]) => ArrayT(base, arr.length)
    } |
      (baseTypes <~> many(arrayBraces) <~ some(whitespace)).map {
        case (base: BaseT, arr: List[String]) if arr.isEmpty => base
        case (base: BaseT, arr: List[String]) => ArrayT(base, arr.length)
      }
  }

  private lazy val pairType: Parsley[PairT] =
    ("pair(" ~> pairElemType <~ "," <~> pairElemType <~ rBracket).map(x => PairT(x._1, x._2))

  private lazy val pairElemType: Parsley[PairElemT] =
    atomic(pairType <~> some(arrayBraces)).map {
      case (base: PairT, arr: List[String]) => ArrayT(base, arr.length)
    } |
      atomic("pair").map(_ => PairNull()) |
      (baseTypes <~> many(arrayBraces)).map {
        case (base: BaseT, arr: List[String]) if arr.isEmpty => base
        case (base: BaseT, arr: List[String]) => ArrayT(base, arr.length)
      }

  private lazy val baseTypes: Parsley[BaseT] = {
    baseType.map(BaseT)
  }

  private lazy val expr: Parsley[Expr] = {
    precedence(atom)(
      Ops(Prefix)("!" as Not),
      Ops(Prefix)(negate as Neg),
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

  private lazy val atom: Parsley[Expr] = {
    "(" ~> expr <~ ")" | atomic(arrayElem).map(x => ArrayElem(Ident(x._1), x._2)) |
      int.map(Num) | ident.map(Ident) | bool.map(Bool) |
      char.map(Ch) | str.map(Str) | pairLiter.map(PairLiter)
  }
}
