package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import parsley.{Success, Result}
import wacc.parser._
import wacc.ASTNodes._

class ParserSpec extends AnyFlatSpec with Matchers {

  "Parser" should "parse basic arithmetic expressions" in {
    val result: Result[String, Expr] = parseTest("1 + 2 * 3")
    result shouldBe Success(Add(Num(1), Mul(Num(2), Num(3))))
  }

  it should "parse expressions with precedence" in {
    val result: Result[String, Expr] = parseTest("(1 + 2) * 3")
    result shouldBe Success(Mul(Add(Num(1), Num(2)), Num(3)))
  }

  it should "parse nested expressions" in {
    val result: Result[String, Expr] = parseTest("(1 + 2) * (3 - 4)")
    result shouldBe Success(Mul(Add(Num(BigInt(1)), Num(BigInt(2))), Sub(Num(BigInt(3)), Num(BigInt(4)))))
  }
}

