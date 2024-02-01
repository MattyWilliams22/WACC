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

  val BEGIN: Parsley[Unit] = lexer.lexeme.symbol("begin")
  val END: Parsley[Unit] = lexer.lexeme.symbol("end")
  val IS: Parsley[Unit] = lexer.lexeme.symbol("is")
  val SKIP: Parsley[Unit] = lexer.lexeme.symbol("skip")
  val READ: Parsley[Unit] = lexer.lexeme.symbol("read")
  val FREE: Parsley[Unit] = lexer.lexeme.symbol("free")
  val RETURN: Parsley[Unit] = lexer.lexeme.symbol("return")
  val EXIT: Parsley[Unit] = lexer.lexeme.symbol("exit")
  val PRINT: Parsley[Unit] = lexer.lexeme.symbol("print")
  val PRINTLN: Parsley[Unit] = lexer.lexeme.symbol("println")
  val IF: Parsley[Unit] = lexer.lexeme.symbol("if")
  val THEN: Parsley[Unit] = lexer.lexeme.symbol("then")
  val ELSE: Parsley[Unit] = lexer.lexeme.symbol("else")
  val FI: Parsley[Unit] = lexer.lexeme.symbol("fi")
  val WHILE: Parsley[Unit] = lexer.lexeme.symbol("while")
  val DO: Parsley[Unit] = lexer.lexeme.symbol("do")
  val DONE: Parsley[Unit] = lexer.lexeme.symbol("done")
  val NEWPAIR: Parsley[Unit] = lexer.lexeme.symbol("newpair")
  val CALL: Parsley[Unit] = lexer.lexeme.symbol("call")
  val FST: Parsley[Unit] = lexer.lexeme.symbol("fst")
  val SND: Parsley[Unit] = lexer.lexeme.symbol("snd")
  val ASSIGN: Parsley[Unit] = lexer.lexeme.symbol("=")
  val STARTBRACKET: Parsley[Unit] = lexer.lexeme.symbol.openParen
  val ENDBRACKET: Parsley[Unit] = lexer.lexeme.symbol.closeParen
  val SEMICOLON: Parsley[Unit] = lexer.lexeme.symbol(";")
  val COMMA: Parsley[Unit] = lexer.lexeme.symbol.comma

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
