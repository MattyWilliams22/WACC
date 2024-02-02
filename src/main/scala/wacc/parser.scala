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


  private lazy val arrayElem = IDENT <~> some("[" ~> expr <~ "]")

  private lazy val types: Parsley[Type] = {
    (pairType <~> many("[]")).map {
      case (base: PairT, arr: List[Unit]) if arr.isEmpty => base
      case (base: PairT, _: List[Unit]) => ArrayT(base)
    } |
      (baseType <~> many("[]")).map {
        case (base: BaseT, arr: List[Unit]) if arr.isEmpty => base
        case (base: BaseT, _: List[Unit]) => ArrayT(base)
      }
  }

  private lazy val baseType: Parsley[BaseT] = {
    BASETYPE.map(BaseT)
  }

  private lazy val pairType: Parsley[PairT] = ("pair(" ~> pairElemType <~ "," <~> pairElemType <~ ")").map(x => PairT(x._1, x._2))

  private lazy val pairElemType: Parsley[PairElemT] =
    atomic(pairType <~> some("[]")).map {
      case (base: PairT, _: List[Unit]) => ArrayT(base)
    } |
      atomic("pair").map(_ => PairNull()) |
      (baseType <~> many("[]")).map {
        case (base: BaseT, arr: List[Unit]) if arr.isEmpty => base
        case (base: BaseT, _: List[Unit]) => ArrayT(base)
      }

  private lazy val prog: Parsley[Prog] = ("begin" ~> many(atomic(func)) <~> stmt <~ "end").map(x => Prog(x._1, x._2))

  private lazy val func: Parsley[Func] = ((((types <~> IDENT <~ "(") <~> option(paramList) <~ ")") <~ "is") <~> stmt <~ "end").map {
    case (((t, i), Some(ps)), s) => Func(t, Ident(i), ps, s)
    case (((t, i), None), s) => Func(t, Ident(i), List(), s)
  }

  private lazy val paramList: Parsley[List[Param]] = (param <~> many("," ~> param)).map(x => x._1 :: x._2)

  private lazy val param: Parsley[Param] = (types <~> IDENT).map(x => Param(x._1, Ident(x._2)))

  private lazy val stmt: Parsley[Stmt] = {
    atomic("skip").map(x => Skip()) |
      declare |
      (lvalue <~> ("=" ~> rvalue)).map(x => Assign(x._1, x._2)) |
      (string("read") ~> lvalue).map(Read) |
      (string("free") <~> expr).map(x => Action(x._1, x._2)) |
      (string("return") <~> expr).map(x => Action(x._1, x._2)) |
      (string("exit") <~> expr).map(x => Action(x._1, x._2)) |
      atomic(string("print") <~> expr).map(x => Action(x._1, x._2)) |
      atomic(string("println") <~> expr).map(x => Action(x._1, x._2)) |
      ifelse |
      (("while" ~> expr <~ "do") <~> stmt <~ "done").map(x => While(x._1, x._2)) |
      ("begin" ~> stmt <~ "end").map(Scope)
      //stmts
  }

  private lazy val declare: Parsley[Declare] = {
    ((types <~> IDENT) <~> ("=" ~> rvalue)).map {
      case ((t, i), r) => Declare(t, Ident(i), r)
    }
  }

  private lazy val ifelse: Parsley[If] = {
    ((("if" ~> expr <~ "then") <~> stmt <~ "else") <~> stmt <~ "fi").map {
      case ((cond, thenS), elseS) => If(cond, thenS, elseS)
    }
  }

  private lazy val stmts: Parsley[Stmts] = (stmt <~> many(";" ~> stmt)).map(x => Stmts(x._1 :: x._2))

  private lazy val pairElem: Parsley[(String, LValue)] = FST <~> lvalue | SND <~> lvalue

  private lazy val lvalue: Parsley[LValue] = {
    IDENT.map(Ident) | atomic(arrayElem).map(x => ArrayElem(Ident(x._1), x._2)) |
      atomic(pairElem).map(x => PairElem(x._1, x._2))
  }

  private lazy val rvalue: Parsley[RValue] = {
    atomic("newpair(" ~> expr <~ "," <~> expr <~ ")").map(x => NewPair(x._1, x._2)) |
      (("call" ~> IDENT <~ "(") <~> option(sepBy(expr, ",")) <~ ")").map {
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
      INT.map(Num) | IDENT.map(Ident) | BOOL.map(Bool) | CHAR.map(Ch) | STR.map(Str) |
      PAIRLITER.map(x => PairLiter())
  }
}
