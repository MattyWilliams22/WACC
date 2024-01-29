package wacc

import parsley.{Parsley, Result}
import parsley.expr.{InfixL, InfixN, InfixR, Ops, Prefix, precedence}
import lexer.implicits.implicitSymbol
import lexer.{bool, char, fully, ident, int, str, pairLiter}

object parser {
    def parse(input: String): Result[String, Expr] = parser.parse(input)
    private val parser = fully(expr)

    sealed trait Expr
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
    case class Var(x: String) extends Expr
    case class Bool(x: String) extends Expr
    case class Ch(x: Char) extends Expr
    case class Str(x: String) extends Expr
    case class PairLiter(x: String) extends Expr

    private lazy val atom: Parsley[Expr] = {
        "(" ~> expr <~ ")" | int.map(Num) | ident.map(Var) | bool.map(Bool) |
          char.map(Ch) | str.map(Str) | pairLiter.map(PairLiter)
    }

    private lazy val expr: Parsley[Expr] = {
        precedence(atom)(
            Ops(Prefix)("!" as Not),
            Ops(Prefix)("-" as Neg),
            Ops(Prefix)("len" as Len),
            Ops(Prefix)("ord" as Ord),
            Ops(Prefix)("chr" as Chr),
            Ops(InfixL)("+" as Add),
            Ops(InfixL)("%" as Mod),
            Ops(InfixL)("/" as Div),
            Ops(InfixL)("*" as Mul),
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
