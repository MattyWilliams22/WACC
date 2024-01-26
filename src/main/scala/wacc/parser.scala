package wacc

import parsley.{Parsley, Result}
import parsley.expr.{precedence, InfixL, Ops}

import lexer.implicits.implicitSymbol
import lexer.{integer, fully, ident}

object parser {
    def parse(input: String): Result[String, BigInt] = parser.parse(input)
    private val parser = fully(expr)

    private lazy val expr: Parsley[BigInt] = {
        precedence(integer, ident as BigInt(10), "(" ~> expr <~ ")")(
            Ops(InfixL)("+" as (_ + _)),
            Ops(InfixL)("-" as (_ - _))
        )
    }
}
