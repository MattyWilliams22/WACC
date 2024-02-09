package wacc

import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource
import scala.io.Source._

import wacc.ASTNodes._
import wacc.Error._

object ErrorOutput {
  val FILE_ERR_CODE = 150
  val SYNTAX_ERR_CODE = 100
  val SEMANTIC_ERR_CODE = 200
  val SUCCESS_CODE = 0

  def output(syntaxError: Option[SyntaxError], file: String, exitStatus: Int): Unit = {
    if (exitStatus == SUCCESS_CODE) {
      println("Program compiled successfully")
      return
    }

    val sb = new StringBuilder()

    if (exitStatus == SYNTAX_ERR_CODE) {
      sb.append("Syntax Error: " + file + "\n")
      val parser = new SyntaxErrorParser(syntaxError.get, file)
      sb.append(parser.parseError())
      println(sb.toString())
      return
    }

    println(sb.toString())

  }

  sealed trait ErrorParser {
    def parseError(): String
  }

  class SyntaxErrorParser(error: SyntaxError, filename: String) extends ErrorParser {
    override def parseError(): String = {
      val sb = new StringBuilder()
      sb.append("Syntax Error: " + filename + " on line " + error.pos._1 + " at column " + error.pos._2 + "\n")

      error.lines match {
        case VanillaError(unexpected, expecteds, reasons, line) =>{
          if (unexpected.isDefined) {
            sb.append("Unexpected token: " + unexpected.get + "\n")
          }
          if (expecteds.nonEmpty) {
            sb.append("Expected tokens: " + expecteds.mkString(", ") + "\n")
          }
          if (reasons.nonEmpty) {
            sb.append("Reasons: " + reasons.mkString(", ") + "\n")
          }
          sb.append(lineInfoToString(line))
        }

        case SpecializedError(msgs, line) => {
          sb.append("Error: " + msgs.mkString(", ") + "\n")
          sb.append(lineInfoToString(line))
        }
      }
      sb.toString()
    }

    def lineInfoToString(line: LineInfo): String = {
      val sb = new StringBuilder()
      if (line.linesBefore.nonEmpty) {
        sb.append("Lines before: \n" + line.linesBefore.mkString("\n ") + "\n")
      }
      sb.append("| " + line.line + "\n")
      sb.append("| " + (" " * line.errorPointsAt) + "^" * math.min(line.errorWidth, (line.line.length - line.errorPointsAt)) + "\n")
      if (line.linesAfter.nonEmpty) {
        sb.append("Lines after: \n" + line.linesAfter.mkString("\n ") + "\n")
      }
      sb.toString()
    }
  }
}