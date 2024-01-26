package wacc

import parsley.{Parsley, Result}
import parsley.expr.{precedence, InfixL, Ops}

import lexer.implicits.implicitSymbol
import lexer.{integer, fully, ident}

object parser {
    def parse(input: String): Result[String, BigInt] = parser.parse(input)
    private val parser = fully(expr)
    
    private val add = (x: BigInt, y: BigInt) => x + y
    private val sub = (x: BigInt, y: BigInt) => x - y

    private lazy val expr: Parsley[BigInt] = {
        precedence(integer, ident as BigInt(10), "(" ~> expr <~ ")")(
            Ops(InfixL)("+" as (_ + _)),
            Ops(InfixL)("-" as (_ - _))
        )
    }
}
