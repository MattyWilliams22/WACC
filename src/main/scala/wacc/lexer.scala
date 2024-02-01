package wacc

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.descriptions._
import parsley.token.descriptions.text.EscapeDesc
import parsley.token.predicate
import parsley.token.descriptions.numeric._
import parsley.syntax.character.stringLift
import parsley.token.symbol.ImplicitSymbol

import scala.language.postfixOps

object lexer {

  val escapes = Set('\\', '\u0000', '\b', '\t', '\n', '\f', '\r', '\"', '\'')

  private val desc = LexicalDesc.plain.copy(
    symbolDesc = SymbolDesc.plain.copy(
       hardKeywords = Set[String]("true", "false", "null", "if", "else", "int",
       "bool", "char", "string", "pair", "begin", "end", "is", "skip",
       "read", "free", "return", "exit", "print", "println", "then", "fi",
       "while", "do", "done", "newpair", "call", "fst", "snd"),
      hardOperators = Set("len", "ord", "chr", "+", "*", "/", "%", "-",
      ">", ">=", "<", "<=", "==", "!=", "&&", "||"),
      caseSensitive = true,
    ),
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(c => c.isLetter || c == '_'),
      identifierLetter = predicate.Basic(c => c.isLetterOrDigit || c == '_'),
    ),
    spaceDesc = SpaceDesc.plain.copy(
      commentLine = "#",
      commentLineAllowsEOF = true,
      space = predicate.Basic(Character.isWhitespace)
    ),
    numericDesc = numeric.NumericDesc.plain.copy(
      integerNumbersCanBeHexadecimal = false,
      integerNumbersCanBeOctal = false,
      decimalExponentDesc = ExponentDesc.NoExponents
    ),
    textDesc = text.TextDesc.plain.copy(
      escapeSequences = EscapeDesc.plain.copy(
        escBegin = '\\',
        literals = Set('\'', '\"', '\\'),
        singleMap = Map(
          '0' -> 0x0000,
          'b' -> 0x0008,
          't' -> 0x0009,
          'n' -> 0x000a,
          'f' -> 0x000c,
          'r' -> 0x000d,
        ),
        gapsSupported = false
      ),
      characterLiteralEnd = '\'',
      stringEnds = Set("\""),
      graphicCharacter = predicate.Basic(c =>
        c >= ' ' && !Set('\\', '\'', '\"').contains(c) || escapes.contains(c)
      )
    )
  )
  private val lexer = new Lexer(desc)

  // Tokens

  val BEGIN: Parsley[String] = lexer.lexeme("begin")
  val END: Parsley[String] = lexer.lexeme("end")
  val IS: Parsley[String] = lexer.lexeme("is")
  val SKIP: Parsley[String] = lexer.lexeme("skip")
  val READ: Parsley[String] = lexer.lexeme("read")
  val FREE: Parsley[String] = lexer.lexeme("free")
  val RETURN: Parsley[String] = lexer.lexeme("return")
  val EXIT: Parsley[String] = lexer.lexeme("exit")
  val PRINT: Parsley[String] = lexer.lexeme("print")
  val PRINTLN: Parsley[String] = lexer.lexeme("println")
  val IF: Parsley[String] = lexer.lexeme("if")
  val THEN: Parsley[String] = lexer.lexeme("then")
  val ELSE: Parsley[String] = lexer.lexeme("else")
  val FI: Parsley[String] = lexer.lexeme("fi")
  val WHILE: Parsley[String] = lexer.lexeme("while")
  val DO: Parsley[String] = lexer.lexeme("do")
  val DONE: Parsley[String] = lexer.lexeme("done")
  val NEWPAIR: Parsley[String] = lexer.lexeme("newpair")
  val CALL: Parsley[String] = lexer.lexeme("call")
  val FST: Parsley[String] = lexer.lexeme("fst")
  val SND: Parsley[String] = lexer.lexeme("snd")
  val ASSIGN: Parsley[String] = lexer.lexeme.symbol("=")
  val STARTBRACKET: Parsley[String] = lexer.lexeme.symbol.openParen
  val ENDBRACKET: Parsley[String] = lexer.lexeme.symbol.closeParen
  val COMMA: Parsley[String] = lexer.lexeme.symbol.comma
  val SEMICOLON: Parsley[String] = lexer.lexeme.symbol.semiColon

  val INT: Parsley[BigInt] = lexer.lexeme.integer.decimal
  val CHAR: Parsley[Char] = lexer.lexeme.character.ascii
  val STR: Parsley[String] = lexer.lexeme.string.ascii
  val BOOL: Parsley[String] = lexer.lexeme("true" | "false")
  val PAIRLITER: Parsley[String] = lexer.lexeme("null")
  val IDENT: Parsley[String] = lexer.lexeme.names.identifier
  val BASETYPE: Parsley[String] = lexer.lexeme("int" | "bool" | "char" | "string" | "pair")

  val OPENSQUARE: Parsley[String] = lexer.lexeme.symbol.openSquare
  val CLOSESQUARE: Parsley[String] = lexer.lexeme.symbol.closeSquare

  val PAIR: Parsley[String] = lexer.lexeme("pair")

  val implicits: ImplicitSymbol = lexer.lexeme.symbol.implicits
  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

  def symbol(s: String): Parsley[String] = lexer.lexeme.symbol(s)
}
