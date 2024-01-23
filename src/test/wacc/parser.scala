package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import parsley.{Success, Failure}
import parsley.{Parsley, Result}
import parsley.expr.chain

class ParserSpec extends AnyFlatSpec {

    "7" should "return 7" in {
        info("Make an input string containing \"7\"")
        val input = "7"
        info("Parse the input string")
        val output = parser.parse(input)
        output shouldBe Success(7)
    }

    "5 + 6" should "return 11" in {
        info("Not implemented!")
        assert(false)
    }

    "9 - 2" should "return 7" in {
        info("Not implemented!")
        assert(false)
    }

    "5 + 6 - 3" should "return 8" in {
        info("Not implemented!")
        assert(false)
    }

    "9 - 2 + 3" should "return 10" in {
        info("Not implemented!")
        assert(false)
    }

    "9 - 2 - 3" should "return 4" in {
        info("Not implemented!")
        assert(false)
    }
}

