package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions._

object lexer {
    private val desc = LexicalDesc.plain.copy(
        // your configuration goes here
    )
    private val lexer = new Lexer(desc)

    val integer = lexer.lexeme.integer.decimal
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
