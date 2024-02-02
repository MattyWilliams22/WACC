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
      lineCommentStart = "#",
      lineCommentAllowsEOF = true,
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
        mapping = Map(
          "0" -> 0x0000,
          "b" -> 0x0008,
          "t" -> 0x0009,
          "n" -> 0x000a,
          "f" -> 0x000c,
          "r" -> 0x000d,
        ),
        gapsSupported = false
      ),
      characterLiteralEnd = '\'',
      stringEnds = Set(("\"", "\"")),
      graphicCharacter = predicate.Basic(c =>
        c >= ' ' && !Set('\\', '\'', '\"').contains(c)
      )
    )
  )
  private val lexer = new Lexer(desc)

  val fst: Parsley[String] = lexer.lexeme("fst")
  val snd: Parsley[String] = lexer.lexeme("snd")
  val print: Parsley[String] = lexer.lexeme("print")
  val println: Parsley[String] = lexer.lexeme("println")
  val read: Parsley[String] = lexer.lexeme("read")
  val free: Parsley[String] = lexer.lexeme("free")
  val ret: Parsley[String] = lexer.lexeme("return")
  val exit: Parsley[String] = lexer.lexeme("exit")
  val begin: Parsley[String] = lexer.lexeme("begin")
  val end: Parsley[String] = lexer.lexeme("end")
  val WHILE: Parsley[String] = lexer.lexeme("while")
  val DO: Parsley[String] = lexer.lexeme("do")
  val done: Parsley[String] = lexer.lexeme("done")
  val IF: Parsley[String] = lexer.lexeme("if")
  val THEN: Parsley[String] = lexer.lexeme("then")
  val ELSE: Parsley[String] = lexer.lexeme("else")
  val fi: Parsley[String] = lexer.lexeme("fi")
  val call: Parsley[String] = lexer.lexeme("call")
  val skip: Parsley[String] = lexer.lexeme("skip")
  val is: Parsley[String] = lexer.lexeme("is")

  val int: Parsley[BigInt] = lexer.lexeme.integer.decimal
  val char: Parsley[Char] = lexer.lexeme.character.ascii
  val str: Parsley[String] = lexer.lexeme.string.ascii
  val bool: Parsley[String] = lexer.lexeme("true" | "false")
  val pairLiter: Parsley[String] = lexer.lexeme("null")
  val ident: Parsley[String] = lexer.lexeme.names.identifier
  val baseType: Parsley[String] = lexer.lexeme("int" | "bool" | "char" | "string" | "pair")

  val implicits: ImplicitSymbol = lexer.lexeme.symbol.implicits
  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
