package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.{Success, Failure}
import parsley.{Parsley, Result}
import parsley.expr.chain

class ParserSpec extends AnyFlatSpec {

    "The string \"7\"" should "return Success(7)" in {
        info("Make an input string containing \"7\"")
        val input = "7"
        info("Parse the input string")
        val output = parser.parse(input)
        info("It should be Success(7)")
        output shouldBe Success(7)
    }

    "The string \"5 + 6\"" should "return Success(11)" in {
        info("Make an input string containing \"5 + 6\"")
        val input = "5 + 6"
        info("Parse the input string")
        val output = parser.parse(input)
        info("It should be Success(11)")
        output shouldBe Success(11)
    }

    "The string \"9 - 2\"" should "return Success(7)" in {
        info("Make an input string containing \"9 - 2\"")
        val input = "9 - 2"
        info("Parse the input string")
        val output = parser.parse(input)
        info("It should be Success(7)")
        output shouldBe Success(7)
    }

    "The string \"5 + 6 - 3\"" should "return Success(8)" in {
        info("Make an input string containing \"5 + 6 - 3\"")
        val input = "5 + 6 - 3"
        info("Parse the input string")
        val output = parser.parse(input)
        info("It should be Success(8)")
        output shouldBe Success(8)
    }

    "The string \"9 - 2 + 3\"" should "return Success(10)" in {
        info("Make an input string containing \"9 - 2 + 3\"")
        val input = "9 - 2 + 3"
        info("Parse the input string")
        val output = parser.parse(input)
        info("It should be Success(10)")
        output shouldBe Success(10)
    }

    "The string \"9 - 2 - 3\"" should "return Success(4)" in {
        info("Make an input string containing \"9 - 2 - 3\"")
        val input = "9 - 2 - 3"
        info("Parse the input string")
        val output = parser.parse(input)
        info("It should be Success(4)")
        output shouldBe Success(4)
    }

    "The string \"+\"" should "return Failure(_)" in {
        info("Make an input string containing \"+\"")
        val input = "+"
        info("Parse the input string")
        val output = parser.parse(input)
        info("It should be Failure(_)")
        output match {
            case Failure(msg) => assert(true)
            case Success(x) => assert(false)
        }
    }

    "The string \"g\"" should "return Failure(_)" in {
        info("Make an input string containing \"g\"")
        val input = "g"
        info("Parse the input string")
        val output = parser.parse(input)
        info("It should be Failure(_)")
        output match {
            case Failure(msg) => assert(true)
            case Success(x) => assert(false)
        }
    }
}

