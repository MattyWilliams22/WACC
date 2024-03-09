package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import parsley.{Success, Failure}

import wacc.ASTNodes._
import wacc.frontend.parser._


class ParserSpec extends AnyFlatSpec with Matchers {

  "The parser" should "parse a simple program" in {
    val input =
      """
        |begin
        | int x = 5;
        | print x
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(),
          Statements(List(
            Declare(BaseT("int"),Ident("x", None, None),Num(5)),Print(Ident("x", None, None))))))
  }

  it should "fail for a program with a missing 'end'" in {
    val input =
      """
        |begin
        | int x = 5;
        | print x
      """.stripMargin.trim

    parse(input) shouldBe a[Failure[_]]
  }

  it should "parse a function with parameters and a return statement" in {
    val input =
      """
        |begin
        | int add(int a, int b) is
        |   return a + b
        | end
        | skip
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(
            Function(
              BaseT("int"), 
              Ident("add", None, None), 
              List(
                Param(BaseT("int"), Ident("a", None, None)), 
                Param(BaseT("int"), Ident("b", None, None))
              ), 
              Statements(List(
                Return(Add(Ident("a", None, None), Ident("b", None, None)))
              ))
            )
          ),
          Statements(List(Skip()))
        )
      )
  }

  it should "fail for a function without 'end'" in {
    val input =
      """
        |begin
        | int add(int a, int b) is
        |   return a + b
        |end
      """.stripMargin.trim

    parse(input) shouldBe a[Failure[_]]
  }

  it should "parse an if-else statement" in {
    val input =
      """
        |begin
        | if true then
        |   print "true"
        | else
        |   print "false"
        | fi
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(), 
          Statements(List(
            If(Bool("true"), 
            Statements(List(
              Print(Str("true"))
            )), 
            Statements(List(
              Print(Str("false"))
            ))
            )
          ))
        )
      )
  }

  it should "parse a while loop" in {
    val input =
      """
        |begin
        | int x = 5;
        | while x > 0 do
        |   print x
        | done
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(), 
          Statements(List(
            Declare(BaseT("int"), Ident("x", None, None), Num(5)),
            While(
              GT(Ident("x", None, None), Num(0)), 
              Statements(List(Print(Ident("x", None, None))))
            )
          ))
        )
      )
  }

  it should "parse a declaration and assignment" in {
    val input =
      """
        |begin
        | int x = 5;
        | x = x + 1
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(), 
          Statements(List(
            Declare(BaseT("int"), Ident("x", None, None), Num(5)), 
            Assign(Ident("x", None, None), Add(Ident("x", None, None), Num(1)))
          ))
        )
      )
  }

  it should "parse a complex expression" in {
    val input =
      """
        |begin
        | int x = 5;
        | int y = 10;
        | int z = (x + y) * 2
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(), 
          Statements(List(
            Declare(BaseT("int"), Ident("x", None, None), Num(5)), 
            Declare(BaseT("int"), Ident("y", None, None), Num(10)), 
            Declare(
              BaseT("int"), 
              Ident("z", None, None), 
              Mul(Add(Ident("x", None, None), Ident("y", None, None)), Num(2))
            )
          ))
        )
      )
  }

  it should "parse an array literal" in {
    val input =
      """
        |begin
        | int[] arr = [1, 2, 3]
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(), 
          Statements(List(
            Declare(
              ArrayT(BaseT("int"), 1), 
              Ident("arr", None, None), 
              ArrayLiter(List(Num(1), Num(2), Num(3)))
            )
          ))
        )
      )
  }

  it should "parse a new pair" in {
    val input =
      """
        |begin
        | pair(int, string) p = newpair(1, "hello")
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(), 
          Statements(List(
            Declare(
              PairT(BaseT("int"), BaseT("string")), 
              Ident("p", None, None), 
              NewPair(Num(1), Str("hello"))
            )
          ))
        )
      )
  }

  it should "parse a call statement with arguments" in {
    val input =
      """
        |begin
        | int add(int a, int b) is
        |  return a + b
        | end
        | int result = call add(3, 4)
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(
            Function(
              BaseT("int"), 
              Ident("add", None, None), 
              List(
                Param(BaseT("int"), Ident("a", None, None)), 
                Param(BaseT("int"), Ident("b", None, None))), 
              Statements(List(
                Return(Add(Ident("a", None, None), Ident("b", None, None)))
              ))
            )
          ), 
          Statements(List(
            Declare(
              BaseT("int"), 
              Ident("result", None, None), 
              Call(Ident("add", None, None), List(Num(3), Num(4)), None)
            )
          ))
        )
      )
  }

  it should "fail for an invalid statement" in {
    val input =
      """
        |begin
        | invalid input
        |end
      """.stripMargin.trim

    parse(input) shouldBe a[Failure[_]]
  }

  it should "parse a complex if-else statement" in {
    val input =
      """
        |begin
        | if x > 0 then
        |   if y > 0 then
        |     print "x and y are positive"
        |   else
        |     print "x is positive, y is non-positive"
        |   fi
        | else
        |   print "x is non-positive"
        | fi
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(), 
          Statements(List(
            If(
              GT(Ident("x", None, None), Num(0)), 
              Statements(List(
                If(
                  GT(Ident("y", None, None), Num(0)), 
                  Statements(List(
                    Print(Str("x and y are positive"))
                  )), 
                  Statements(List(
                    Print(Str("x is positive, y is non-positive"))
                  ))
                )
              )), 
              Statements(List(Print(Str("x is non-positive"))))
            )
          ))
        )
      )
  }

  it should "parse nested while loops" in {
    val input =
      """
        |begin
        | while x > 0 do
        |   while y > 0 do
        |     print "nested while loop"
        |   done;
        |   x = x - 1
        | done
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(), 
          Statements(List(
            While(
              GT(Ident("x", None, None), Num(0)), 
              Statements(List(
                While(
                  GT(Ident("y", None, None), Num(0)), 
                  Statements(List(
                    Print(Str("nested while loop"))
                  ))
                ), 
                Assign(Ident("x", None, None), Sub(Ident("x", None, None), Num(1)))
              ))
            )
          ))
        )
      )
  }

  it should "fail for an incomplete if-else statement" in {
    val input =
      """
        |begin
        | if x > 0 then
        |   print "x is positive"
        |end
      """.stripMargin.trim

    parse(input) shouldBe a[Failure[_]]
  }

  it should "parse a function with no parameters and return statement" in {
    val input =
      """
        |begin
        | int simpleFunc() is
        |   return 42
        | end
        | skip
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(
            Function(
              BaseT("int"), 
              Ident("simpleFunc", None, None), 
              List(), 
              Statements(List(
                Return(Num(42))
              ))
            )
          ), 
          Statements(List(Skip()))
        )
      )
  }

  // This is syntactically correct but semantically incorrect
  it should "parse a string concatenation expression" in {
    val input =
      """
        |begin
        | stringConcat = "Hello, " + "World!"
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(), 
          Statements(List(
            Assign(
              Ident("stringConcat", None, None),
              Add(Str("Hello, "), Str("World!"))
            )
          ))
        )
      )
  }

  it should "fail for a program with a missing semicolon between statements" in {
    val input =
      """
        |begin
        | int x = 5
        | bool flag = true
        |end
      """.stripMargin.trim

    parse(input) shouldBe a[Failure[_]]
  }

  it should "parse a program with a nested block" in {
    val input =
      """
        |begin
        | int x = 5;
        | begin
        |   bool flag = true;
        |   if flag then
        |     print "Flag is true"
        |   else
        |     print "Flag is false"
        |   fi
        | end
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(), 
          Statements(List(
            Declare(BaseT("int"), Ident("x", None, None), Num(5)), 
            Scope(
              Statements(List(
                Declare(BaseT("bool"), Ident("flag", None, None), Bool("true")), 
                If(
                  Ident("flag", None, None), 
                  Statements(List(
                    Print(Str("Flag is true"))
                  )), 
                  Statements(List(
                    Print(Str("Flag is false"))
                  ))
                )
              ))
            )
          ))
        )
      )
  }

  it should "fail for an invalid pair type declaration" in {
    val input =
      """
        |begin
        | pair invalid = pair(int, bool);
        |end
      """.stripMargin.trim

    parse(input) shouldBe a[Failure[_]]
  }

  it should "parse a program with logical expressions" in {
    val input =
      """
        |begin
        | bool result = (x > 0 && y <= 10) || (z == 42)
        |end
      """.stripMargin.trim

    parse(input) shouldBe 
      Success(
        Program(
          List(), 
          Statements(List(
            Declare(
              BaseT("bool"), 
              Ident("result", None, None), 
              Or(
                And(
                  GT(Ident("x", None, None), Num(0)), 
                  LTEQ(Ident("y", None, None), Num(10))
                ), 
                EQ(Ident("z", None, None), Num(42))
              )
            )
          ))
        )
      )
  }

  it should "fail for an invalid if-else statement" in {
    val input =
      """
        |begin
        | if x > 0 then
        |   print "x is positive";
        | else
        |   print "x is non-positive"
        | fi
        |end
      """.stripMargin.trim

    parse(input) shouldBe a[Failure[_]]
  }

}

