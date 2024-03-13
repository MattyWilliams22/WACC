import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import wacc.extensions.Optimiser._
import wacc.ASTNodes._

class ControlFlowAnalysisSpec extends AnyFlatSpec with Matchers {
  "ControlFlowAnalysis" should "correctly analyse a program with a constant TRUE condition" in {
    
    val program = 
    Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            If(
                Bool("true"),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(10)))),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(-3))))), 
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    
    val output = controlFlowOptimise(program)
    
    output should be (Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            Statements(List(
                Assign(
                    Ident("x", None, Some(BaseT("int"))),
                    Num(10)))),
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    )
  }

  it should "correctly analyse a program with a constant FALSE condition" in {
    val program = 
    Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            If(
                Bool("false"),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(10)))),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(-3))))), 
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    
    val output = controlFlowOptimise(program)
    
    output should be (Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            Statements(List(
                Assign(
                    Ident("x", None, Some(BaseT("int"))),
                    Num(-3)))),
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    )
  }

  it should "correctly analyse a program with a constant AND condition" in {
    val program = 
    Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            If(
                And(
                    Bool("true"),
                    Bool("true")),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(10)))),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(-3))))), 
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    
    val output = controlFlowOptimise(program)
    
    output should be (Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            Statements(List(
                Assign(
                    Ident("x", None, Some(BaseT("int"))),
                    Num(10)))),
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    )
  }

  it should "correctly analyse a program with a constant OR condition" in {
    val program = 
    Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            If(
                Or(
                    Bool("true"),
                    Bool("false")),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(10)))),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(-3))))), 
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    
    val output = controlFlowOptimise(program)
    
    output should be (Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            Statements(List(
                Assign(
                    Ident("x", None, Some(BaseT("int"))),
                    Num(10)))),
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    )
  }

  it should "correctly analyse a program with a constant NOT condition" in {
    val program = 
    Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            If(
                Not(Bool("true")),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(10)))),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(-3))))), 
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    
    val output = controlFlowOptimise(program)
    
    output should be (Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            Statements(List(
                Assign(
                    Ident("x", None, Some(BaseT("int"))),
                    Num(-3)))),
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    )
  }

  it should "correctly analyse a program with a constant EQ condition" in {
    val program = 
    Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            If(
                EQ(
                    Num(1),
                    Num(1)),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(10)))),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(-3))))), 
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    
    val output = controlFlowOptimise(program)
    
    output should be (Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            Statements(List(
                Assign(
                    Ident("x", None, Some(BaseT("int"))),
                    Num(10)))),
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    )
  }

  it should "correctly analyse a program with a constant NEQ condition" in {
    val program = 
    Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            If(
                NEQ(
                    Num(1),
                    Num(1)),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(10)))),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(-3))))), 
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    
    val output = controlFlowOptimise(program)
    
    output should be (Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            Statements(List(
                Assign(
                    Ident("x", None, Some(BaseT("int"))),
                    Num(-3)))),
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    )
  }

  it should "correctly analyse a program with a EQ condition that uses the same variable twice" in {
    val program = 
    Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            If(
                EQ(
                    Ident("x", None, Some(BaseT("int"))),
                    Ident("x", None, Some(BaseT("int")))),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(10)))),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Num(-3))))), 
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    
    val output = controlFlowOptimise(program)
    
    output should be (Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            Statements(List(
                Assign(
                    Ident("x", None, Some(BaseT("int"))),
                    Num(10)))),
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    )
  }

  it should "correctly analyse a program with a constant FALSE condition in a while loop and remove the while loop" in {
    
    val program = 
    Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            While(
                Bool("false"),
                Statements(List(
                    Assign(
                        Ident("x", None, Some(BaseT("int"))),
                        Add(
                            Ident("x", None, Some(BaseT("int"))),
                                Num(1)))))),
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    
    val output = controlFlowOptimise(program)
    
    output should be (Program(
        List(),
        Statements(List(
            Declare(
                BaseT("int"),
                Ident("x", None, Some(BaseT("int"))),
                Num(1)), 
            Skip(),
            Println(
                Ident("x", None, Some(BaseT("int")))))))
    )
  }
}
