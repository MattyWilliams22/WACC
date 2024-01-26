package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions._
import parsley.token.descriptions.text.EscapeDesc
import parsley.token.predicate

object lexer {
    private val desc = LexicalDesc.plain.copy(
        symbolDesc = SymbolDesc.plain.copy(
            hardKeywords = Set("true", "false", "null", "if", "else", "int",
            "bool", "char", "string", "pair", "begin", "end", "is", "skip",
            "read", "free", "return", "exit", "print", "println", "then", "fi",
            "while", "do", "done", "newpair", "call", "fst", "snd"),
            hardOperators = Set("len", "ord", "chr", "+", "*", "/", "%", "-",
            ">", ">=", "<", "<=", "==", "!=", "&&", "||")
        ),
        nameDesc = NameDesc.plain.copy(
            identifierStart = predicate.Basic(c => c.isLetter | c == '_'),
            identifierLetter = predicate.Basic(c => c.isLetterOrDigit | c == '_')
        ),
        spaceDesc = SpaceDesc.plain.copy(
            lineCommentStart = "#",
            lineCommentAllowsEOF = true
        ),
        numericDesc = numeric.NumericDesc.plain.copy(

        ),
        textDesc = text.TextDesc.plain.copy(
            escapeSequences = EscapeDesc.plain.copy(
                escBegin = '\\',
                literals = Set('0', 'b', 't', 'n', 'f', 'r', '"', '\'', '\\'),
            )
        )
    )
    private val lexer = new Lexer(desc)

    val integer = lexer.lexeme.integer.decimal
    val ident = lexer.lexeme.names.identifier
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
