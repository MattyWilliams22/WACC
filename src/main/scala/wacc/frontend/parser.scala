package wacc.frontend

import parsley.Parsley.{atomic, many, some}
import parsley.character.whitespace
import parsley.combinator.{option, sepBy, sepBy1}
import parsley.expr._
import parsley.{Parsley, Result}
import wacc.ASTNodes._
import wacc.frontend.Error._
import wacc.frontend.lexer._
import wacc.frontend.lexer.implicits.implicitSymbol


object parser {
  implicit val errorHandler: SyntaxErrorBuilder = new SyntaxErrorBuilder

  def parse(input: String): Result[SyntaxError, Program] = parser.parse(input)
  private [wacc] def parseTest(input: String): Result[SyntaxError, Expr] = parserTest.parse(input)

  /* To be able to run tests only on expression */
  private lazy val parserTest = fully(expr)
  private val parser = fully(prog)

  /* ‘begin’ ⟨func⟩* ⟨stmt⟩ ‘end’ */
  private lazy val prog: Parsley[Program] =
    (begin ~> many(func) <~> statement <~ end).map(x => Program(x._1, x._2))

  /* ⟨type⟩ ⟨ident⟩ ‘(’ ⟨param-list⟩? ‘)’ ‘is’ ⟨stmt⟩ ‘end’ */
  private lazy val func: Parsley[Function] =
    ((atomic(types <~> ident <~ "(") <~> paramList <~ ")" <~ is) <~>
      statement.filter(bodyHasReturnOrExit) <~ end).map {
      case (((t, i), ps), s) => Function(t, Ident(i), ps, s)
    }

  /* Checks if the body of a function has a return or exit statement */
  private def bodyHasReturnOrExit(s: Statement): Boolean = s match {
    case Statements(x) => bodyHasReturnOrExit(x.last)
    case If(_, thenS, elseS) => bodyHasReturnOrExit(thenS) && bodyHasReturnOrExit(elseS)
    case While(_, body) => bodyHasReturnOrExit(body)
    case Scope(body) => bodyHasReturnOrExit(body)
    case Return(_) | Exit(_) => true
    case _ => false
  }

  /* ⟨param⟩ ( ‘,’ ⟨param⟩ )* */
  private lazy val paramList: Parsley[List[Param]] =
    sepBy(param, ",")

  /* ⟨type⟩ ⟨ident⟩ */
  private lazy val param: Parsley[Param] = (types <~> ident).map(x => Param(x._1, Ident(x._2)))

  /* ‘skip’
  | ⟨lvalue⟩ ‘=’ ⟨rvalue⟩
  | ‘read’ ⟨lvalue⟩
  | ‘free’ ⟨expr⟩
  | ‘return’ ⟨expr⟩
  | ‘exit’ ⟨expr⟩
  | ‘print’ ⟨expr⟩
  | ‘println’ ⟨expr⟩
  | ‘while’ ⟨expr⟩ ‘do’ ⟨stmt⟩ ‘done’
  | ‘begin’ ⟨stmt⟩ ‘end’
  | ⟨stmt⟩ ‘;’ ⟨stmt⟩ */
  private lazy val statement: Parsley[Statement] = {
    sepBy1(atomic(skip).map(_ => Skip()) |
      atomic(read ~> lvalue).map(Read) |
      atomic(free ~> expr).map(Free) |
      atomic(ret ~> expr).map(Return) |
      atomic(exit ~> expr).map(Exit) |
      atomic(println ~> expr).map(Println) |
      atomic(print ~> expr).map(Print) |
      atomic(ifElse) |
      atomic(declare) |
      atomic(lvalue <~> ("=" ~> rvalue)).map(x => Assign(x._1, x._2)) |
      atomic((WHILE ~> expr <~ DO) <~> statement <~ done).map(x => While(x._1, x._2)) |
      atomic(begin ~> statement <~ end).map(Scope), ";").map(Statements)
  }

  /* ‘if’ ⟨expr⟩ ‘then’ ⟨stmt⟩ ‘else’ ⟨stmt⟩ ‘fi’ */
  private lazy val ifElse: Parsley[If] = {
    (((IF ~> expr <~ THEN) <~> statement <~ ELSE) <~> statement <~ fi).map {
      case ((cond, thenS), elseS) => If(cond, thenS, elseS)
    }
  }

  /* ⟨type⟩ ⟨ident⟩ ‘=’ ⟨rvalue⟩ */
  private lazy val declare: Parsley[Declare] = {
    ((types <~> ident) <~> ("=" ~> rvalue)).map {
      case ((t, i), r) => Declare(t, Ident(i), r)
    }
  }

  /* ⟨ident⟩ | ⟨array-elem⟩ | ⟨pair-elem⟩ */
  private lazy val lvalue: Parsley[LValue] = {
    atomic(arrayElem).map(x => ArrayElem(Ident(x._1), x._2)) |
      atomic(pairElem).map(x => PairElem(x._1, x._2)) | atomic(ident).map(Ident)
  }

  /* ⟨expr⟩
  | ⟨array-liter⟩
  | ‘newpair’ ‘(’ ⟨expr⟩ ‘,’ ⟨expr⟩ ‘)’
  | ⟨pair-elem⟩
  | ‘call’ ⟨ident⟩ ‘(’ ⟨⟨expr⟩ (‘,’ ⟨expr⟩ )*⟩? ‘)’ */
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

  /* ⟨ident⟩ (‘[’ ⟨expr⟩ ‘]’)+ */
  private lazy val arrayElem = ident <~> some("[" ~> expr <~ "]")

  /* ‘fst’ ⟨lvalue⟩ | ‘snd’ ⟨lvalue⟩ */
  private lazy val pairElem: Parsley[(String, LValue)] = fst <~> lvalue | snd <~> lvalue

  /* ⟨base-type⟩ | ⟨⟨type⟩ ‘[’ ‘]’⟩ | ⟨pair-type⟩ */
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

  /* ‘pair’ ‘(’ ⟨pair-elem-type⟩ ‘,’ ⟨pair-elem-type⟩ ‘)’ */
  private lazy val pairType: Parsley[PairT] =
    ("pair(" ~> pairElemType <~ "," <~> pairElemType <~ rBracket).map(x => PairT(x._1, x._2))

  /* = ⟨base-type⟩ | ⟨⟨type⟩ ‘[’ ‘]’⟩ | ‘pair’ */
  private lazy val pairElemType: Parsley[PairElemT] =
    atomic(pairType <~> some(arrayBraces)).map {
      case (base: PairT, arr: List[String]) => ArrayT(base, arr.length)
    } |
      atomic("pair").map(_ => PairNull()) |
      (baseTypes <~> many(arrayBraces)).map {
        case (base: BaseT, arr: List[String]) if arr.isEmpty => base
        case (base: BaseT, arr: List[String]) => ArrayT(base, arr.length)
      }

  /* ‘int’ | ‘bool’ | ‘char’ | ‘string’ */
  private lazy val baseTypes: Parsley[BaseT] = {
    baseType.map(BaseT)
  }

  /* ⟨unary-oper⟩ ⟨expr⟩
  | ⟨expr⟩ ⟨binary-oper⟩ ⟨expr⟩
  | ⟨atom⟩ */
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

  /* ⟨int-liter⟩
  | ⟨bool-liter⟩
  | ⟨char-liter⟩
  | ⟨str-liter⟩
  | ⟨pair-liter⟩
  | ⟨ident⟩
  | ⟨array-elem⟩
  | ‘(’ ⟨expr⟩ ‘)’ */
  private lazy val atom: Parsley[Expr] = {
    "(" ~> expr <~ ")" | atomic(arrayElem).map(x => ArrayElem(Ident(x._1), x._2)) |
      int.map(Num) | ident.map(Ident) | bool.map(Bool) |
      char.map(Ch) | str.map(Str) | pairLiter.map(PairLiter)
  }
}
