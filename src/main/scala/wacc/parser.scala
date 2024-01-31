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

    private val parser = fully(prog)

    // Need 0 or more funcs
    private lazy val prog: Parsley[Prog] = ("begin" ~> func <~> stmt <~ "end").map(x => Prog(List(x._1), x._2))

    private lazy val func: Parsley[Func] = ???

    private lazy val stmt: Parsley[Stmt] = {
            string("skip").map(x => Skip()) |
            declare |
            (lvalue <~> ("=" ~> rvalue)).map(x => Assign(x._1, x._2)) |
            (string("read") ~> lvalue).map(Read) |
            (string("free") <~> expr).map(x => Action(x._1, x._2)) |
            (string("return") <~> expr).map(x => Action(x._1, x._2)) |
            (string("exit") <~> expr).map(x => Action(x._1, x._2)) |
            (string("print") <~> expr).map(x => Action(x._1, x._2)) |
            (string("println") <~> expr).map(x => Action(x._1, x._2)) |
            ifelse |
            (("while" ~> expr <~ "do") <~> stmt <~ "done").map(x => While(x._1, x._2)) |
            ("begin" ~> stmt <~ "end").map(Scope) |
            stmts
    }

    private lazy val declare: Parsley[Declare] = {
        ((types <~> ident) <~> ("=" ~> rvalue)).map {
            case ((t, i), r) => Declare(t, Ident(i), r)
        }
    }

    private lazy val ifelse: Parsley[If] = {
        ((("if" ~> expr <~ "then") <~> stmt <~ "else") <~> stmt <~ "fi").map {
            case ((cond, thenS), elseS) => If(cond, thenS, elseS)
        }
    }

    private lazy val stmts: Parsley[Stmt] = ???

    private lazy val arrayElem = ident <~> some("[" ~> expr <~ "]")

    private lazy val pairElem = string("fst") <~> lvalue | string("snd") <~> lvalue

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

    private lazy val types: Parsley[Type] = {
        baseType | arrayType | pairType
    }

    private lazy val baseType: Parsley[BaseT] = {
        string("int").map(BaseT) | string("bool").map(BaseT) | string("char").map(BaseT) | string("string").map(BaseT)
    }

    private lazy val arrayType: Parsley[ArrayT] = (types <~ "[]").map(ArrayT)

    private lazy val pairType: Parsley[PairT] = ("pair" ~> ("(" ~> pairElemType <~ ",") <~> pairElemType <~ ")").map(x => PairT(x._1, x._2))

    private lazy val pairElemType: Parsley[PairElemT] = baseType | arrayType | string("pair").map(x => PairNull())

    private lazy val atom: Parsley[Expr] = {
        "(" ~> expr <~ ")" | atomic(arrayElem).map(x => ArrayElem(Ident(x._1), x._2)) |
            int.map(Num) | ident.map(Ident) | bool.map(Bool) | char.map(Ch) | str.map(Str) |
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
