package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.io.{ByteArrayOutputStream, File, PrintStream}

class MainSpec extends AnyFlatSpec with Matchers {

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
}

