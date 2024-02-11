package wacc

import java.io.{ByteArrayOutputStream, File, PrintStream}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MainSpec extends AnyFlatSpec with Matchers {
  System.setSecurityManager(new NoExitSecurityManager)

  "Main" should "return success code for valid program" in {
    val exception = intercept[SecurityException] {
      Main.main(Array("begin skip end"))
    }
    exception.getMessage should include("Program compiled successfully")
  }

  it should "return syntax error" in {
    val exception = intercept[SecurityException] {
      Main.main(Array("begin end"))
    }
    exception.getMessage should include("Syntax Error")
  }

  it should "return file does not exist error" in {
    val exception = intercept[SecurityException] {
      Main.main(Array("nonexistent.wacc"))
    }
    exception.getMessage should include("File does not exist")
  }

  it should "return semantic error" in {
    val exception = intercept[SecurityException] {
      Main.main(Array("begin string x = 5 end"))
    }
    exception.getMessage should include("Semantic Error")
  }
}

