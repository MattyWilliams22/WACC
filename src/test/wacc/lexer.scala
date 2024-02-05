package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parsley.{Success, Result, Parsley, Failure}
import wacc.lexer._

class LexerSpec extends AnyFlatSpec with Matchers {

  "Lexer" should "correctly parse 'fst'" in {
    val successInput = "fst"
    val failureInput = "notfst"

    val successResult: Result[String, String] = lexer.fst.parse(successInput)
    val failureResult: Result[String, String] = lexer.fst.parse(failureInput)

    successResult shouldBe Success("fst")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'snd'" in {
    val successInput = "snd"
    val failureInput = "notsnd"

    val successResult: Result[String, String] = lexer.snd.parse(successInput)
    val failureResult: Result[String, String] = lexer.snd.parse(failureInput)

    successResult shouldBe Success("snd")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'print'" in {
    val successInput = "print"
    val failureInput = "notprint"

    val successResult: Result[String, String] = lexer.print.parse(successInput)
    val failureResult: Result[String, String] = lexer.print.parse(failureInput)

    successResult shouldBe Success("print")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'println'" in {
    val successInput = "println"
    val failureInput = "notprintln"

    val successResult: Result[String, String] = lexer.println.parse(successInput)
    val failureResult: Result[String, String] = lexer.println.parse(failureInput)

    successResult shouldBe Success("println")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'read'" in {
    val successInput = "read"
    val failureInput = "notread"

    val successResult: Result[String, String] = lexer.read.parse(successInput)
    val failureResult: Result[String, String] = lexer.read.parse(failureInput)

    successResult shouldBe Success("read")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'free'" in {
    val successInput = "free"
    val failureInput = "notfree"

    val successResult: Result[String, String] = lexer.free.parse(successInput)
    val failureResult: Result[String, String] = lexer.free.parse(failureInput)

    successResult shouldBe Success("free")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'return'" in {
    val successInput = "return"
    val failureInput = "notreturn"

    val successResult: Result[String, String] = lexer.ret.parse(successInput)
    val failureResult: Result[String, String] = lexer.ret.parse(failureInput)

    successResult shouldBe Success("return")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'exit'" in {
    val successInput = "exit"
    val failureInput = "notexit"

    val successResult: Result[String, String] = lexer.exit.parse(successInput)
    val failureResult: Result[String, String] = lexer.exit.parse(failureInput)

    successResult shouldBe Success("exit")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'begin'" in {
    val successInput = "begin"
    val failureInput = "notbegin"

    val successResult: Result[String, String] = lexer.begin.parse(successInput)
    val failureResult: Result[String, String] = lexer.begin.parse(failureInput)

    successResult shouldBe Success("begin")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'end'" in {
    val successInput = "end"
    val failureInput = "notend"

    val successResult: Result[String, String] = lexer.end.parse(successInput)
    val failureResult: Result[String, String] = lexer.end.parse(failureInput)

    successResult shouldBe Success("end")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'while'" in {
    val successInput = "while"
    val failureInput = "notwhile"

    val successResult: Result[String, String] = lexer.WHILE.parse(successInput)
    val failureResult: Result[String, String] = lexer.WHILE.parse(failureInput)

    successResult shouldBe Success("while")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'do'" in {
    val successInput = "do"
    val failureInput = "notdo"

    val successResult: Result[String, String] = lexer.DO.parse(successInput)
    val failureResult: Result[String, String] = lexer.DO.parse(failureInput)

    successResult shouldBe Success("do")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'done'" in {
    val successInput = "done"
    val failureInput = "notdone"

    val successResult: Result[String, String] = lexer.done.parse(successInput)
    val failureResult: Result[String, String] = lexer.done.parse(failureInput)

    successResult shouldBe Success("done")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'if'" in {
    val successInput = "if"
    val failureInput = "notif"

    val successResult: Result[String, String] = lexer.IF.parse(successInput)
    val failureResult: Result[String, String] = lexer.IF.parse(failureInput)

    successResult shouldBe Success("if")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'then'" in {
    val successInput = "then"
    val failureInput = "notthen"

    val successResult: Result[String, String] = lexer.THEN.parse(successInput)
    val failureResult: Result[String, String] = lexer.THEN.parse(failureInput)

    successResult shouldBe Success("then")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'else'" in {
    val successInput = "else"
    val failureInput = "notelse"

    val successResult: Result[String, String] = lexer.ELSE.parse(successInput)
    val failureResult: Result[String, String] = lexer.ELSE.parse(failureInput)

    successResult shouldBe Success("else")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'fi'" in {
    val successInput = "fi"
    val failureInput = "notfi"

    val successResult: Result[String, String] = lexer.fi.parse(successInput)
    val failureResult: Result[String, String] = lexer.fi.parse(failureInput)

    successResult shouldBe Success("fi")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'call'" in {
    val successInput = "call"
    val failureInput = "notcall"

    val successResult: Result[String, String] = lexer.call.parse(successInput)
    val failureResult: Result[String, String] = lexer.call.parse(failureInput)

    successResult shouldBe Success("call")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'skip'" in {
    val successInput = "skip"
    val failureInput = "notskip"

    val successResult: Result[String, String] = lexer.skip.parse(successInput)
    val failureResult: Result[String, String] = lexer.skip.parse(failureInput)

    successResult shouldBe Success("skip")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse 'is'" in {
    val successInput = "is"
    val failureInput = "notis"

    val successResult: Result[String, String] = lexer.is.parse(successInput)
    val failureResult: Result[String, String] = lexer.is.parse(failureInput)

    successResult shouldBe Success("is")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse integer literals" in {
    val successInput = "123"
    val failureInput = "not123"

    val successResult: Result[String, Int] = lexer.int.parse(successInput)
    val failureResult: Result[String, Int] = lexer.int.parse(failureInput)

    successResult shouldBe Success(123)
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse character literals" in {
    val successInput = "'a'"
    val failureInput = "'ab'"

    val successResult: Result[String, Char] = lexer.char.parse(successInput)
    val failureResult: Result[String, Char] = lexer.char.parse(failureInput)

    successResult shouldBe Success('a')
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse string literals" in {
    val successInput = "\"hello\""
    val failureInput = "\"hello"

    val successResult: Result[String, String] = lexer.str.parse(successInput)
    val failureResult: Result[String, String] = lexer.str.parse(failureInput)

    successResult shouldBe Success("hello")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse boolean literals" in {
    val successInput = "true"
    val failureInput = "nottrue"

    val successResult: Result[String, String] = lexer.bool.parse(successInput)
    val failureResult: Result[String, String] = lexer.bool.parse(failureInput)

    successResult shouldBe Success("true")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse null literals" in {
    val successInput = "null"
    val failureInput = "notnull"

    val successResult: Result[String, String] = lexer.pairLiter.parse(successInput)
    val failureResult: Result[String, String] = lexer.pairLiter.parse(failureInput)

    successResult shouldBe Success("null")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse identifiers" in {
    val successInput = "variable"
    val failureInput = "123variable"

    val successResult: Result[String, String] = lexer.ident.parse(successInput)
    val failureResult: Result[String, String] = lexer.ident.parse(failureInput)

    successResult shouldBe Success("variable")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse base types" in {
    val successInput = "int"
    val failureInput = "notint"

    val successResult: Result[String, String] = lexer.baseType.parse(successInput)
    val failureResult: Result[String, String] = lexer.baseType.parse(failureInput)

    successResult shouldBe Success("int")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse array braces" in {
    val successInput = "[]"
    val failureInput = "[notclosed"

    val successResult: Result[String, String] = lexer.arrayBraces.parse(successInput)
    val failureResult: Result[String, String] = lexer.arrayBraces.parse(failureInput)

    successResult shouldBe Success("[]")
    failureResult shouldBe a[Failure[_]]
  }

  it should "correctly parse right brackets" in {
    val successInput = ")"
    val failureInput = "(notclosed"

    val successResult: Result[String, String] = lexer.rBracket.parse(successInput)
    val failureResult: Result[String, String] = lexer.rBracket.parse(failureInput)

    successResult shouldBe Success(")")
    failureResult shouldBe a[Failure[_]]
  }
}

