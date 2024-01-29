package wacc

import parsley.{Failure, Success}

import java.io.File
import scala.io.Source

object Main {
  private val FILE_ERR_CODE = 150

  def main(args: Array[String]): Unit = {
    println("Hello WACC!")

    if (args.length < 1) {
      println("Please enter an expression!")
      System.exit(FILE_ERR_CODE)
    }

    var input = ""
    val arg = args(0)

    if (arg.endsWith(".wacc")) {
      val inputFile = new File(args(0))

      /* Checks if file exists*/
      if (!inputFile.exists()) {
        println("File does not exist")
        System.exit(FILE_ERR_CODE)
      }

      /* Checks if input string is a file */
      if (!inputFile.isFile) {
        println("It's not a file")
        System.exit(FILE_ERR_CODE)
      }

      /*Reads file contents */
      val source = Source.fromFile(inputFile)
      val fileContents = try source.mkString finally source.close()
      input = fileContents
    } else {
      input = arg
    }

    /* Parsing of expression */
    parser.parse(input) match {
      case Success(x) => println(s"$arg = $x")
      case Failure(msg) => println(msg)
    }
  }
}
