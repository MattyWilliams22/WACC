package wacc

import parsley.{Failure, Success, Result}

import java.io.File
import scala.io.Source
import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.MatchParserDemand
import wacc.ASTNodes._

object Main {
  val FILE_ERR_CODE = 150
  val SYNTAX_ERR_CODE = 100
  val SEMANTIC_ERR_CODE = 200

  
  case class SyntaxError(pos: (Int, Int), expected: Set[String], found: String)

  case class VanillaError(
      unexpected: Option[String],
      expecteds: Set[String],
      reasons: Set[String]) 
  case class SpecializedError(msgs: Set[String]) 

  class SyntaxErrorBuilder extends ErrorBuilder[SyntaxError] with MatchParserDemand {
    type Position = (Int, Int)
    type Source = Unit
    type ErrorInfoLines = SyntaxError
    type Item = String
    type Raw = String
    type Named = String
    type EndOfInput = String
    type Message = String
    type Messages = Set[String]
    type ExpectedItems = Set[String]
    type ExpectedLine = Set[String]
    type UnexpectedLine = Option[String]
    type LineInfo = Unit

  def format(pos: (Int, Int), source: Unit, error: SyntaxError): SyntaxError = {
    val (line, col) = pos
    val message = error match {
      case SyntaxError(_, expected, found) =>
        s"Expected one of: ${expected.mkString(", ")}, found: $found at line $line, column $col."
    }
    SyntaxError(pos, Set(), message)
  }

  def vanillaError(unexpected: Option[String], expected: Set[String], reasons: Set[String], line: Unit): SyntaxError =
    SyntaxError((0, 0), expected, unexpected.getOrElse("unknown"))

  def specializedError(msgs: Set[String], line: Unit): SyntaxError =
    SyntaxError((0, 0), Set.empty, msgs.headOption.getOrElse("unknown"))

  def pos(line: Int, col: Int): (Int, Int) = (line, col)
  def source(sourceName: Option[String]): Unit = ()
  def combineExpectedItems(alts: Set[String]): Set[String] = alts
  def combineMessages(alts: Seq[String]): Set[String] = alts.toSet
  def unexpected(item: Option[String]): Option[String] = item
  def expected(alts: Set[String]): Set[String] = alts
  def message(msg: String): String = msg
  def reason(msg: String): String = msg
  def raw(item: String): String = item
  def named(item: String): String = item
  val endOfInput: String = "end of input"
  val numLinesAfter: Int = 0
  val numLinesBefore: Int = 0
  def lineInfo(
      line: String,
      linesBefore: Seq[String],
      linesAfter: Seq[String],
      errorPointsAt: Int, errorWidth: Int
    ): Unit = ()
  }

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
      implicit val errorBuilder: ErrorBuilder[SyntaxError] = new SyntaxErrorBuilder
      val result: Result[SyntaxError, Expr] = parser.parseTest(input)
      result match {
        case Success(x) => println(s"$arg = $x")
        case Failure(msg) => println(msg)
      }
    } else {
      // Invoke your parser's parse method
      implicit val errorBuilder: ErrorBuilder[SyntaxError] = new SyntaxErrorBuilder
      val result: Result[SyntaxError, Program] = parser.parse(input)
      /* Parsing of expression */
      result match {
        case Success(x) => {
          // Semantic Analysis
          println(s"$arg = $x")
          System.exit(0)
        }
        case Failure(msg) => {
          println(msg)
          System.exit(SYNTAX_ERR_CODE)
        }
      }
    }
  }
}
