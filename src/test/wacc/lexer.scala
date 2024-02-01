package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parsley.{Success, Result}
import wacc.lexer._

class LexerSpec extends AnyFlatSpec with Matchers {

  "Lexer" should "lex integers" in {
    val result = INT.parse("123")
    result shouldBe Success(BigInt(123))
  }

  it should "lex characters" in {
    val result = CHAR.parse("'a'")
    result shouldBe Success('a')
  }

  it should "lex strings" in {
    val result = STR.parse("\"hello\"")
    result shouldBe Success("hello")
  }

  it should "lex booleans" in {
    val trueResult = BOOL.parse("true")
    trueResult shouldBe Success("true")

    val falseResult = BOOL.parse("false")
    falseResult shouldBe Success("false")
  }

  it should "lex null" in {
    val result = PAIRLITER.parse("null")
    result shouldBe Success("null")
  }

  it should "lex identifiers" in {
    val result = IDENT.parse("variableName")
    result shouldBe Success("variableName")
  }

  it should "fully parse a given parser" in {
    val result = fully(IDENT).parse("fullyParsed")
    result shouldBe Success("fullyParsed")
  }
}

