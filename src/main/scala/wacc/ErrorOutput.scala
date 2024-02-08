package wacc

import Error._
import ASTNodes._

import scala.collection.mutable.ListBuffer
import scala.io.BufferedSource
import scala.io.Source._

object ErrorOutput {
  val FILE_ERR_CODE = 150
  val SYNTAX_ERR_CODE = 100
  val SEMANTIC_ERR_CODE = 200
  val SUCCESS_CODE = 0

  def output(log: ListBuffer[SemanticError], syntaxError: Option[SyntaxError], file: String, exitStatus: Int): Unit = {
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
      sb.append("Syntax Error: " + filename + "on line " + error.pos._1 + " at column " + error.pos._2 + "\n")

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
          sb.append("Line: " + lineInfoToString(line) + "\n")
          sb.append("\n")
        }

        case SpecializedError(msgs, line) => {
          sb.append("Error: " + msgs.mkString(", ") + "\n")
          sb.append("Line: " + lineInfoToString(line) + "\n")
          sb.append("\n")
        }
      }
      sb.append("\n")
      sb.toString()
    }

    def lineInfoToString(line: LineInfo): String = {
      val sb = new StringBuilder()
      sb.append("Lines before: \n" + line.linesBefore.mkString("\n ") + "\n")
      sb.append("| " + line.line + "\n")
      sb.append("| " + (" " * line.errorPointsAt) + "^" * math.min(line.errorWidth, (line.line.length - line.errorPointsAt)) + "\n")
      sb.append("Lines after: \n" + line.linesAfter.mkString("\n ") + "\n")
      sb.toString()
    }
  }

  class SemanticErrorParser(log: ListBuffer[SemanticError], filename: String) extends ErrorParser {
    override def parseError(): String = {
      val sb = new StringBuilder()
      sb.append("Semantic Error: " + filename + "\n")
      for (error <- log) {
        error match {
          case UnknownIdentifierError(pos, ident, context) => {
            sb.append("Unknown identifier: " + ident + " at line " + pos._1 + " at column " + pos._2 + "\n")
          }

          case TypeError(pos, expected, actual, context) => {
            sb.append("Type Error: Expected " + expected + " but got " + actual + " at line " + pos._1 + " at column " + pos._2 + "\n")
            if (context.isDefined) {
              sb.append("Context: " + context.get + "\n")
            }
          }

          case TypeErasureError(pos, context) => {
            sb.append("Type Erasure Error at line " + pos._1 + " at column " + pos._2 + "\n")
            if (context.isDefined) {
              sb.append("Context: " + context.get + "\n")
            }
          }

          case UnknownObjectError(pos, context) => {
            sb.append("Unknown object at line " + pos._1 + " at column " + pos._2 + "\n")
            if (context.isDefined) {
              sb.append("Context: " + context.get + "\n")
            }
          }

          case InvalidScopeError(pos, member, context) => {
            sb.append("Invalid scope: " + member + " at line " + pos._1 + " at column " + pos._2 + "\n")
            if (context.isDefined) {
              sb.append("Context: " + context.get + "\n")
            }
          }

          case ArityMismatch(pos, expected, actual, context) => {
            sb.append("Arity Mismatch: Expected " + expected + " but got " + actual + " at line " + pos._1 + " at column " + pos._2 + "\n")
            if (context.isDefined) {
              sb.append("Context: " + context.get + "\n")
            }
          }

          case ArrayError(pos, name, maxDim, context) => {
            sb.append("Array Error: " + name + " has " + maxDim + " dimensions at line " + pos._1 + " at column " + pos._2 + "\n")
            if (context.isDefined) {
              sb.append("Context: " + context.get + "\n")
            }
          }

          case DuplicateIdentifier(pos, ident, context) => {
            sb.append("Duplicate Identifier: " + ident + " at line " + pos._1 + " at column " + pos._2 + "\n")
            if (context.isDefined) {
              sb.append("Context: " + context.get + "\n")
            }
          }

          case InvalidReturnError(pos, context) => {
            sb.append("Invalid Return at line " + pos._1 + " at column " + pos._2 + "\n")
            if (context.isDefined) {
              sb.append("Context: " + context.get + "\n")
            }
          }
        }
      }
      sb.toString()
    }
  }
}