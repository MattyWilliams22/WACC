package wacc

import parsley.{Failure, Success, Result}

import java.io.File
import scala.io.Source
import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.MatchParserDemand
import wacc.ASTNodes._
import scala.collection.mutable.ListBuffer

import ErrorOutput._
import Error._

object Main {

  def main(args: Array[String]): Unit = {
    println("Hello WACC!")

    /* Exits if arguments are not defined */
    if (args.length < 1) {
      println("Please enter an expression!")
      System.exit(FILE_ERR_CODE)
    }

    var input = ""
    val arg = args(0)

    /* Parses files only if argument ends with ".wacc" */
    if(arg.endsWith(".wacc")) {
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
    }
    else {
      /* Parses string instead */
      input = arg
    }


    /* To be able to run Tests */
    if (args.length > 1) {
      // Invoke your parser's parse method
      val result: Result[SyntaxError, Expr] = parser.parseTest(input)
      result match {
        case Success(x) => println(s"$arg = $x")
        case Failure(msg) => println(msg)
      }
    } else {
      // Invoke your parser's parse method
      val result: Result[SyntaxError, Program] = parser.parse(input)
      /* Parsing of expression */
      result match {
        case Success(x) => {
          // Semantic Analysis
          println(s"$arg = $x")
          System.exit(0)
        }
        case Failure(msg) => {
          output(ListBuffer.empty[Error.SemanticError], Some(msg), args(0), SYNTAX_ERR_CODE)
          System.exit(SYNTAX_ERR_CODE)
        }
      }
    }
  }
}
