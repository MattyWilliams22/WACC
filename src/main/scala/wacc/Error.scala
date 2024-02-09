package wacc

import parsley.errors.ErrorBuilder
import parsley.errors.tokenextractors.MatchParserDemand

import wacc.ASTNodes.Type

object Error {

  // Semantic or Syntax Error
  sealed trait ErrorType

  case class SyntaxError(pos: (Int, Int), source: Option[String], lines: ErrorLines) extends ErrorType

  sealed trait ErrorLines
  case class VanillaError(
      unexpected: Option[String],
      expecteds: Set[String],
      reasons: List[String],
      line: LineInfo) extends ErrorLines

  case class SpecializedError(msgs: List[String], line:LineInfo) extends ErrorLines

  case class LineInfo(line: String, linesBefore: Seq[String], linesAfter: Seq[String], errorPointsAt: Int, errorWidth: Int)


  class SyntaxErrorBuilder extends ErrorBuilder[SyntaxError] with MatchParserDemand {
    override type Position = (Int, Int)
    override type Source = Option[String]
    override type ErrorInfoLines = ErrorLines
    override type Item = String
    override type Raw = String
    override type Named = String
    override type EndOfInput = String
    override type Message = String
    override type Messages = List[String]
    override type ExpectedItems = Set[String]
    override type ExpectedLine = Set[String]
    override type UnexpectedLine = Option[String]
    override type LineInfo = Error.LineInfo

    override def format(pos: Position, source: Source, lines: ErrorLines): SyntaxError = {
      SyntaxError(pos, source, lines)
    }

    override def vanillaError(unexpected: Option[String], expected: Set[String], reasons: List[String], line: LineInfo): ErrorInfoLines =
      VanillaError(unexpected, expected, reasons, line)

    override def specializedError(msgs: List[String], line: LineInfo): ErrorInfoLines =
      SpecializedError(msgs, line)

    def pos(line: Int, col: Int): Position = (line, col)
    def source(sourceName: Option[String]): Source = sourceName
    def combineExpectedItems(alts: Set[String]): ExpectedItems = alts
    def combineMessages(alts: Seq[Message]): Messages = alts.toList
    def unexpected(item: Option[String]): UnexpectedLine = item
    def expected(alts: ExpectedItems): Set[String] = alts
    def message(msg: String): Message = msg
    def reason(rsn: String): Message = rsn
    def raw(item: String): Raw = item
    def named(item: String): Named = item
    val endOfInput: EndOfInput = "end of input"
    val numLinesAfter: Int = 3
    val numLinesBefore: Int = 3
    def lineInfo(
        line: String,
        linesBefore: Seq[String],
        linesAfter: Seq[String],
        errorPointsAt: Int, errorWidth: Int
      ): LineInfo = LineInfo(line, linesBefore, linesAfter, errorPointsAt, errorWidth)
  }
}