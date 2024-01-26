package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions._
import parsley.token.predicate

object lexer {
    private val desc = LexicalDesc.plain.copy(
        symbolDesc = SymbolDesc.plain.copy(

        ),
        nameDesc = NameDesc.plain.copy(
            identifierStart = predicate.Basic(c => c.isLetter | c == '_'),
            identifierLetter = predicate.Basic(c => c.isLetterOrDigit | c == '_'),
        ),
        spaceDesc = SpaceDesc.plain.copy(

        ),
        numericDesc = numeric.NumericDesc.plain.copy(

        ),
        textDesc = text.TextDesc.plain.copy(

        ),
    )
    private val lexer = new Lexer(desc)

    val integer = lexer.lexeme.integer.decimal
    val ident = lexer.lexeme.names.identifier
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
