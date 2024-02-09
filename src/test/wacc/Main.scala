package wacc

import java.io.{ByteArrayOutputStream, File, PrintStream}

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MainSpec extends AnyFlatSpec with Matchers {
  System.setSecurityManager(new NoExitSecurityManager)

  private def captureOutput(block: => Unit): String = {
    val outContent = new ByteArrayOutputStream()
    Console.withOut(new PrintStream(outContent)) {
      block
    }
    outContent.toString.trim
  }

  "Main" should "parse and print expressions from files" in {
    val testFileContent = "6 + 7"
    val testFile = File.createTempFile("test", ".wacc")
    val fileOutput = captureOutput {
      java.nio.file.Files.write(testFile.toPath, testFileContent.getBytes)
      Main.main(Array(testFile.getAbsolutePath, "test"))
    }
    fileOutput should include("Add(Num(6),Num(7))")
  }

  it should "parse and print expressions from command-line arguments" in {
    val output = captureOutput {
      Main.main(Array("5 + 10", "test"))
    }
    output should include("5 + 10 = Add(Num(5),Num(10))")
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

  it should "return success code for valid program" in {
    val exception = intercept[SecurityException] {
      Main.main(Array("begin int x = 5 end"))
    }
    exception.getMessage should include("Program compiled successfully")
  }
}

