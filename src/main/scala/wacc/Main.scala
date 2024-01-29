package wacc

import parsley.{Failure, Success}

import java.io.File
import scala.io.Source

object Main {
  private val FILE_ERR_CODE = 150

  def main(args: Array[String]): Unit = {
    println("hello WACC!")

    if (args.length < 1) {
      println("please enter an expression")
      System.exit(FILE_ERR_CODE)
    }

    if (args(0).endsWith(".wacc")) {

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

      /* Parsing result */
      val parseResult = parser.parse(fileContents)
      parseResult match {
        case Success(x) => println(s"$parseResult = $x")
        case Failure(msg) => println(msg)
      }
    } else {
      /* Parsing of expression */
      parser.parse(args(0)) match {
        case Success(x) => println(s"$args(0) = $x")
        case Failure(msg) => println(msg)
      }
    }
  }
}
