package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions._

object lexer {
    private val desc = LexicalDesc.plain.copy(
        symbolDesc = SymbolDesc.plain.copy(

        ),
        nameDesc = NameDesc.plain.copy(

        ),
        spaceDesc = SpaceDesc.plain.copy(

        ),
        numericDesc = numeric.NumericDesc.plain.copy(

        ),
        textDesc = text.TextDesc.plain.copy(

        ),
    )
    private val lexer = new Lexer(desc)

    val integer = lexer.lexeme.integer.decimal32
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
