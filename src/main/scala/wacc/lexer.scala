package wacc

import scala.language.postfixOps

import parsley.character.digit
import parsley.errors.combinator.ErrorMethods
import parsley.Parsley
import parsley.Parsley.{atomic, notFollowedBy}
import parsley.syntax.character.stringLift
import parsley.token.descriptions._
import parsley.token.descriptions.numeric._
import parsley.token.descriptions.text.EscapeDesc
import parsley.token.errors._
import parsley.token.Lexer
import parsley.token.predicate
import parsley.token.symbol.ImplicitSymbol

object lexer {
  val errConfig = new ErrorConfig {
    override def labelSymbol = Map(
      "}" -> LabelAndReason(
        label = "closing brace",
        reason = "unclosed braces"
      ),
      "]" -> LabelAndReason(
        label = "closing square bracket",
        reason = "unclosed square brackets"
      ),
      "len" -> Label("unary operator"),
      "ord" -> Label("unary operator"),
      "chr" -> Label("unary operator"),
      "!" -> Label("unary operator"),
      "*" -> Label("binary operator"),
      "/" -> Label("binary operator"),
      "%" -> Label("binary operator"),
      "+" -> Label("binary operator"),
      ">" -> Label("binary operator"),
      ">=" -> Label("binary operator"),
      "<" -> Label("binary operator"),
      "<=" -> Label("binary operator"),
      "==" -> Label("binary operator"),
      "!=" -> Label("binary operator"),
      "&&" -> Label("binary operator"),
      "||" -> Label("binary operator"),
      "-" -> Label("binary operator"),
    )
  }

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
        literals = Set.empty,
        mapping = Map(
          "0" -> 0x0000,
          "b" -> 0x0008,
          "t" -> 0x0009,
          "n" -> 0x000a,
          "f" -> 0x000c,
          "r" -> 0x000d,
          "\"" -> 0x0022,
          "\'" -> 0x0027,
          "\\" -> 0x005c
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

  private val lexer = new Lexer(desc, errConfig)

  val fst: Parsley[String] = lexer.lexeme("fst").label("pair element")
  val snd: Parsley[String] = lexer.lexeme("snd").label("pair element")
  val print: Parsley[String] = lexer.lexeme("print").label("action statement")
  val println: Parsley[String] = lexer.lexeme("println").label("action statement")
  val read: Parsley[String] = lexer.lexeme("read").label("action statement")
  val free: Parsley[String] = lexer.lexeme("free").label("action statement")
  val ret: Parsley[String] = lexer.lexeme("return").label("action statement")
  val exit: Parsley[String] = lexer.lexeme("exit").label("action statement")
  val begin: Parsley[String] = lexer.lexeme("begin").label("start of scope")
  val end: Parsley[String] = lexer.lexeme("end")explain("unclosed scope or function")
  val WHILE: Parsley[String] = lexer.lexeme("while").label("while loop")
  val DO: Parsley[String] = lexer.lexeme("do")
  val done: Parsley[String] = lexer.lexeme("done").label("end of while loop").explain("unclosed while loop")
  val IF: Parsley[String] = lexer.lexeme("if").label("if statement")
  val THEN: Parsley[String] = lexer.lexeme("then")
  val ELSE: Parsley[String] = lexer.lexeme("else").explain("no else clause")
  val fi: Parsley[String] = lexer.lexeme("fi").label("end of if statement").explain("unclosed if statement")
  val call: Parsley[String] = lexer.lexeme("call").label("function call")
  val skip: Parsley[String] = lexer.lexeme("skip")
  val is: Parsley[String] = lexer.lexeme("is")
  // One of these tokens breaks the parser
  // val pair: Parsley[String] = lexer.lexeme("pair").label("pair type")
  // val pairdef: Parsley[String] = lexer.lexeme("pair(").label("pair definition")
  // val newpairdef: Parsley[String] = lexer.lexeme("newpair(").label("newpair definition")

  val int: Parsley[Int] = lexer.lexeme.integer.decimal32.label("integer literal")
  val char: Parsley[Char] = lexer.lexeme.character.ascii.label("character literal")
  val str: Parsley[String] = lexer.lexeme.string.ascii.label("string literal")
  val bool: Parsley[String] = lexer.lexeme("true" | "false").label("boolean literal")
  val pairLiter: Parsley[String] = lexer.lexeme("null")
  val ident: Parsley[String] = lexer.lexeme.names.identifier.label("identifier")
  val baseType: Parsley[String] = ("int" | "bool" | "char" | "string").label("base type")
  val arrayBraces: Parsley[String] = "[]"
  val rBracket: Parsley[String] = ")"
  val negate: Parsley[Unit] = atomic("-" ~> notFollowedBy(digit)).label("unary operator")

  val implicits: ImplicitSymbol = lexer.lexeme.symbol.implicits
  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
