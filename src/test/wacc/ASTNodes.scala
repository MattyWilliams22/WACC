package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import wacc.ASTNodes._

class ASTNodesSpec extends AnyFlatSpec with Matchers {
  // Test for Program
  "Program" should "check functions and statement" in {
    val program = Program(List(), Skip())
    program.check() shouldBe true
  }

  it should "return false if any function is invalid" in {
    val invalidFunction = Function(BaseT("invalid"), Ident("func"), List(), Skip())
    val program = Program(List(invalidFunction), Skip())
    program.check() shouldBe false
  }

  it should "return false if the statement is invalid" in {
    val program = Program(List(), Assign(Ident("x"), Num(5)))
    program.check() shouldBe false
  }

  // Test for Function
  "Function" should "check type, identifier, parameters, and body" ignore {
    val func = Function(BaseT("int"), Ident("func"), List(), Skip())
    func.check() shouldBe true
  }

  it should "return false if type or identifier is invalid" in {
    val invalidFunc = Function(BaseT("invalid"), Ident("func"), List(), Skip())
    invalidFunc.check() shouldBe false
  }

  // Test for Param
  "Param" should "check type and identifier" ignore {
    val param = Param(BaseT("int"), Ident("param"))
    param.check() shouldBe true
  }

  it should "return false if type or identifier is invalid" in {
    val invalidParam = Param(BaseT("invalid"), Ident("param"))
    invalidParam.check() shouldBe false
  }

  // Test for Skip
  "Skip" should "return true for check()" in {
    val skip = Skip()
    skip.check() shouldBe true
  }

  // Test for Declare
  "Declare" should "check type, identifier, and value" ignore {
    val declare = Declare(BaseT("int"), Ident("x"), Num(5))
    declare.check() shouldBe true
  }

  it should "return false if type, identifier, or value is invalid" in {
    val invalidDeclare = Declare(BaseT("invalid"), Ident("x"), Num(5))
    invalidDeclare.check() shouldBe false
  }

  // Test for Assign
  "Assign" should "check lvalue and rvalue" ignore {
    val assign = Assign(Ident("x"), Num(5))
    assign.check() shouldBe true
  }

  it should "return false if lvalue or rvalue is invalid" in {
    val invalidAssign = Assign(Ident("x"), Bool("int"))
    invalidAssign.check() shouldBe false
  }

  // Test for Read
  "Read" should "check lvalue" ignore {
    val read = Read(Ident("x"))
    read.check() shouldBe true
  }

  it should "return false if lvalue is invalid" in {
    val invalidRead = Read(Ident("x$"))
    invalidRead.check() shouldBe false
  }

  // Test for If
  "If" should "check condition, thenS, and elseS" in {
    val ifStatement = If(Bool("true"), Skip(), Skip())
    ifStatement.check() shouldBe true
  }

  it should "return false if condition, thenS, or elseS is invalid" in {
    val invalidIfStatement = If(Num(5), Skip(), Skip())
    invalidIfStatement.check() shouldBe false
  }

  // Test for While
  "While" should "check condition and body" in {
    val whileStatement = While(Bool("true"), Skip())
    whileStatement.check() shouldBe true
  }

  it should "return false if condition or body is invalid" in {
    val invalidWhileStatement = While(Num(5), Skip())
    invalidWhileStatement.check() shouldBe false
  }

  // Test for Scope
  "Scope" should "check body" in {
    val scope = Scope(Skip())
    scope.check() shouldBe true
  }

  it should "return false if body is invalid" in {
    val invalidScope = Scope(Assign(Ident("x"), Bool("4")))
    invalidScope.check() shouldBe false
  }

  // Test for Statements
  "Statements" should "check all statements in the list" ignore {
    val statements = Statements(List(Skip(), Assign(Ident("x"), Num(5))))
    statements.check() shouldBe true
  }

  it should "return false if any statement in the list is invalid" in {
    val invalidStatements = Statements(List(Skip(), Assign(Ident("x"), Bool("correct"))))
    invalidStatements.check() shouldBe false
  }

  // Test for Free
  "Free" should "check expression is an array or pair" ignore {
    val freeStatement = Free(ArrayElem(Ident("arr"), List(Num(0))))
    freeStatement.check() shouldBe true
  }

  it should "return false if expression is not an array or pair" in {
    val invalidFreeStatement = Free(Num(5))
    invalidFreeStatement.check() shouldBe false
  }

  // Test for Return
  "Return" should "check expression" in {
    val returnStatement = Return(Num(5))
    returnStatement.check() shouldBe true
  }

  it should "return false if expression is invalid" in {
    val invalidReturnStatement = Return(Bool("0"))
    invalidReturnStatement.check() shouldBe false
  }

  // Test for Exit
  "Exit" should "check expression" in {
    val exitStatement = Exit(Num(0))
    exitStatement.check() shouldBe true
  }

  it should "return false if expression is invalid" in {
    val invalidExitStatement = Exit(Bool("1"))
    invalidExitStatement.check() shouldBe false
  }

  // Test for Print
  "Print" should "check expression" in {
    val printStatement = Print(Str("Hello"))
    printStatement.check() shouldBe true
  }

  it should "return false if expression is invalid" in {
    val invalidPrintStatement = Print(Bool("y"))
    invalidPrintStatement.check() shouldBe false
  }

  // Test for Println
  "Println" should "check expression" in {
    val printlnStatement = Println(Str("World"))
    printlnStatement.check() shouldBe true
  }

  it should "return false if expression is invalid" in {
    val invalidPrintlnStatement = Println(Bool("z"))
    invalidPrintlnStatement.check() shouldBe false
  }

  // Test for And
  "And" should "check both expressions" in {
    val andExpression = And(Bool("true"), Bool("false"))
    andExpression.check() shouldBe true
  }

  it should "return false if either expression is invalid" in {
    val invalidAndExpression = And(Bool("true"), Num(5))
    invalidAndExpression.check() shouldBe false
  }

  // Test for Or
  "Or" should "check both expressions" in {
    val orExpression = Or(Bool("true"), Bool("false"))
    orExpression.check() shouldBe true
  }

  it should "return false if either expression is invalid" in {
    val invalidOrExpression = Or(Bool("true"), Num(5))
    invalidOrExpression.check() shouldBe false
  }

  // Test for Not
  "Not" should "check the expression" in {
    val notExpression = Not(Bool("true"))
    notExpression.check() shouldBe true
  }

  it should "return false if the expression is invalid" in {
    val invalidNotExpression = Not(Num(5))
    invalidNotExpression.check() shouldBe false
  }

  // Test for Neg
  "Neg" should "check the expression" in {
    val negExpression = Neg(Num(5))
    negExpression.check() shouldBe true
  }

  it should "return false if the expression is invalid" in {
    val invalidNegExpression = Neg(Bool("true"))
    invalidNegExpression.check() shouldBe false
  }

  // Test for Len
  "Len" should "check the expression" ignore {
    val lenExpression = Len(ArrayElem(Ident("arr"), List(Num(0))))
    lenExpression.check() shouldBe true
  }

  it should "return false if the expression is invalid" in {
    val invalidLenExpression = Len(Bool("true"))
    invalidLenExpression.check() shouldBe false
  }

  // Test for Ord
  "Ord" should "check the expression" in {
    val ordExpression = Ord(Ch('a'))
    ordExpression.check() shouldBe true
  }

  it should "return false if the expression is invalid" in {
    val invalidOrdExpression = Ord(Bool("false"))
    invalidOrdExpression.check() shouldBe false
  }

  // Test for Chr
  "Chr" should "check the expression" in {
    val chrExpression = Chr(Num(97))
    chrExpression.check() shouldBe true
  }

  it should "return false if the expression is invalid" in {
    val invalidChrExpression = Chr(Bool("true"))
    invalidChrExpression.check() shouldBe false
  }
}
