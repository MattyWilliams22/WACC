package wacc

import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import wacc.ASTNodes._

class ASTNodesSpec extends AnyFlatSpec with Matchers with BeforeAndAfterEach {
  override def beforeEach(): Unit = {
    currentSymbolTable = new SymbolTable(None)
    semanticErrors.clear()
  }

  override protected def afterEach(): Unit = {
    currentSymbolTable = new SymbolTable(None)
    semanticErrors.clear()
  }

  // Test for Program
  "Program" should "check functions and statement" in {
    val program = Program(List(), Skip())
    // All Programs call exit
    program.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if any function is invalid" in {
    val invalidFunction = Function(BaseT("invalid"), Ident("func", None, None), List(), Skip())
    val program = Program(List(invalidFunction), Skip())
    program.check()
    semanticErrors.toList should not be empty
  }

  it should "return false if the statement is invalid" in {
    val program = Program(List(), Assign(Ident("x", None, None), Num(5)))
    program.check()
    semanticErrors.toList should not be empty
  }

  // Test for Function
  "Function" should "return false if type or identifier is invalid" in {
    val invalidFunc = Function(BaseT("invalid"), Ident("func", None, None), List(), Skip())
    invalidFunc.check()
    semanticErrors.toList should not be empty
  }

  // Test for Param
  "Param" should "return false if type or identifier is invalid" in {
    val invalidParam = Param(BaseT("invalid"), Ident("param", None, None))
    invalidParam.check()
    semanticErrors.toList should not be empty
  }

  // Test for Skip
  "Skip" should "return true for check()" in {
    val skip = Skip()
    skip.check()
    semanticErrors.toList shouldBe empty
  }

  // Test for Declare
  "Declare" should "check type, identifier, and value" in {
    val declare = Declare(BaseT("int"), Ident("x", None, None), Num(5))
    declare.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if type, identifier or value is invalid" in {
    val invalidDeclare = Declare(BaseT("invalid"), Ident("x", None, None), Num(5))
    invalidDeclare.check()
    semanticErrors.toList should not be empty
  }

  // Test for Assign
  "Assign" should "check lvalue and rvalue" in {
    val assign = Assign(Ident("x", None, None), Num(5))
    assign.check() 
    semanticErrors.toList should not be empty
  }

  it should "return false if lvalue or rvalue is invalid" in {
    val invalidAssign = Assign(Ident("x", None, None), Bool("int"))
    invalidAssign.check()
    semanticErrors.toList should not be empty
  }

  // Test for Read
  "Read" should "check lvalue" in {
    val declare = Declare(BaseT("int"), Ident("x", None, None), Num(5))
    declare.check()
    val read = Read(Ident("x", None, None))
    read.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if lvalue is invalid" in {
    val invalidRead = Read(Ident("x", None, None))
    invalidRead.check()
    semanticErrors.toList should not be empty
  }

  // Test for If
  "If" should "check condition, thenS, and elseS" in {
    val ifStatement = If(Bool("true"), Skip(), Skip())
    ifStatement.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if condition, thenS, or elseS is invalid" in {
    val invalidIfStatement = If(Num(5), Skip(), Skip())
    invalidIfStatement.check()
    semanticErrors.toList should not be empty
  }

  // Test for While
  "While" should "check condition and body" in {
    val whileStatement = While(Bool("true"), Skip())
    whileStatement.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if condition or body is invalid" in {
    val invalidWhileStatement = While(Num(5), Skip())
    invalidWhileStatement.check()
    semanticErrors.toList should not be empty
  }

  // Test for Scope
  "Scope" should "check body" in {
    val scope = Scope(Skip())
    scope.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if body is invalid" in {
    val invalidScope = Scope(Assign(Ident("x", None, None), Bool("4")))
    invalidScope.check()
    semanticErrors.toList should not be empty
  }

  // Test for Statements
  "Statements" should "check all statements in the list" in {
    val declare = Declare(BaseT("int"), Ident("x", None, None), Num(8))
    declare.check()
    val statements = Statements(List(Skip(), Assign(Ident("x", None, None), Num(5))))
    statements.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if any statement in the list is invalid" in {
    val invalidStatements = Statements(List(Skip(), Assign(Ident("x", None, None), Bool("correct"))))
    invalidStatements.check()
    semanticErrors.toList should not be empty
  }

  // Test for Free
  "Free" should "return false if expression is not an array or pair" in {
    val invalidFreeStatement = Free(Num(5))
    invalidFreeStatement.check()
    semanticErrors.toList should not be empty
  }

  // Test for Return
  "Return" should "check expression" in {
    val returnStatement = Return(Num(5))
    returnStatement.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if expression is invalid" in {
    val invalidReturnStatement = Return(Bool("0"))
    invalidReturnStatement.check()
    semanticErrors.toList should not be empty
  }

  // Test for Exit
  "Exit" should "check expression" in {
    val exitStatement = Exit(Num(0))
    exitStatement.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if expression is invalid" in {
    val invalidExitStatement = Exit(Bool("1"))
    invalidExitStatement.check()
    semanticErrors.toList should not be empty
  }

  // Test for Print
  "Print" should "check expression" in {
    val printStatement = Print(Str("Hello"))
    printStatement.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if expression is invalid" in {
    val invalidPrintStatement = Print(Bool("y"))
    invalidPrintStatement.check()
    semanticErrors.toList should not be empty
  }

  // Test for Println
  "Println" should "check expression" in {
    val printlnStatement = Println(Str("World"))
    printlnStatement.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if expression is invalid" in {
    val invalidPrintlnStatement = Println(Bool("z"))
    invalidPrintlnStatement.check()
    semanticErrors.toList should not be empty
  }

  // Test for And
  "And" should "check both expressions" in {
    val andExpression = And(Bool("true"), Bool("false"))
    andExpression.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if either expression is invalid" in {
    val invalidAndExpression = And(Bool("true"), Num(5))
    invalidAndExpression.check()
    semanticErrors.toList should not be empty
  }

  // Test for Or
  "Or" should "check both expressions" in {
    val orExpression = Or(Bool("true"), Bool("false"))
    orExpression.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if either expression is invalid" in {
    val invalidOrExpression = Or(Bool("true"), Num(5))
    invalidOrExpression.check()
    semanticErrors.toList should not be empty
  }

  // Test for Not
  "Not" should "check the expression" in {
    val notExpression = Not(Bool("true"))
    notExpression.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if the expression is invalid" in {
    val invalidNotExpression = Not(Num(5))
    invalidNotExpression.check()
    semanticErrors.toList should not be empty
  }

  // Test for Neg
  "Neg" should "check the expression" in {
    val negExpression = Neg(Num(5))
    negExpression.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if the expression is invalid" in {
    val invalidNegExpression = Neg(Bool("true"))
    invalidNegExpression.check()
    semanticErrors.toList should not be empty
  }

  // Test for Len

  "Len" should "return false if the expression is invalid" in {
    val invalidLenExpression = Len(Bool("true"))
    invalidLenExpression.check()
    semanticErrors.toList should not be empty
  }

  // Test for Ord
  "Ord" should "check the expression" in {
    val ordExpression = Ord(Ch('a'))
    ordExpression.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if the expression is invalid" in {
    val invalidOrdExpression = Ord(Bool("false"))
    invalidOrdExpression.check()
    semanticErrors.toList should not be empty
  }

  // Test for Chr
  "Chr" should "check the expression" in {
    val chrExpression = Chr(Num(97))
    chrExpression.check()
    semanticErrors.toList shouldBe empty
  }

  it should "return false if the expression is invalid" in {
    val invalidChrExpression = Chr(Bool("true"))
    invalidChrExpression.check()
    semanticErrors.toList should not be empty
  }
}
