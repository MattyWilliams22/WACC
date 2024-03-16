import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ListBuffer

import wacc.extensions.Optimiser._
import wacc.ASTNodes._
import wacc.backend._

class ControlFlowAnalysisSpec extends AnyFlatSpec with Matchers {
  "ControlFlowAnalysis" should "correctly analyse a program with a constant TRUE condition" in {
    
    val program: Program = 
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
    
    val output: Program = controlFlowOptimise(program)
    
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
    val program: Program = 
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
    
    val output: Program = controlFlowOptimise(program)
    
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
    val program: Program = 
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
    
    val output: Program = controlFlowOptimise(program)
    
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
    val program: Program = 
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
    
    val output: Program = controlFlowOptimise(program)
    
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
    val program: Program = 
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
    
    val output: Program = controlFlowOptimise(program)
    
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
    val program: Program = 
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
    
    val output: Program = controlFlowOptimise(program)
    
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
    val program: Program = 
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
    
    val output: Program = controlFlowOptimise(program)
    
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
    val program: Program = 
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
    
    val output: Program = controlFlowOptimise(program)
    
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
    
    val program: Program = 
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
    
    val output: Program = controlFlowOptimise(program)
    
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

  it should "correctly analyse a program with an unused function" in {
    
    val program: ListBuffer[Instruction] = ListBuffer(
			Comment("Start of program", 4),
			Command("data", 0),
			Command("align 4", 0),
			Command("text", 0),
			Command("global main", 0),
			Label("main"),
			Push(List(FP, LR)),
			Mov(FP, SP, noCondition),
			Comment("Start of declare", 4),
			Comment("Start of identifier", 4),
			Mov(R5, R5, noCondition),
			Comment("End of identifier", 4),
			Comment("Start of number", 4),
			Mov(R5, ImmVal(5), noCondition),
			Comment("Start of assign", 4),
			Comment("Start of function call", 4),
			Push(List(R1, R2, R3)),
			Comment("Start of identifier", 4),
			Mov(R6, R5, noCondition),
			Comment("End of identifier", 4),
			Mov(R0, R6, noCondition),
			BInstr("wacc_f_0", noCondition, true),
			Pop(List(R1, R2, R3)),
			Mov(R4, R0, noCondition),
			Comment("End of function call", 4),
			Mov(R5, R4, noCondition),
			Comment("End of assign", 4),
			Comment("Start of print", 4),
			Comment("Start of identifier", 4),
			Mov(R4, R5, noCondition),
			Comment("End of identifier", 4),
			Comment("Print Logic", 4),
			Mov(R0, R4, noCondition),
			BInstr("_printi", noCondition, true),
			BInstr("_println", noCondition, true),
			Mov(R0, ImmVal(0), noCondition),
			Pop(List(FP, PC)),
			NewLine(),
			Comment("Start of function", 4),
			Label("wacc_f_0"),
			Push(List(FP, LR)),
			Push(List(R4, R5, R6, R7, R8, R9, R10)),
			Mov(FP, SP, noCondition),
			SubInstr(SP, SP, ImmVal(0), false),
			Mov(R5, R0, noCondition),
			Push(List(R0, R1, R2, R3)),
			Comment("Start of return", 4),
			Comment("Start of addition expression", 4),
			Comment("Start of number", 4),
			Mov(R4, ImmVal(1), noCondition),
			Push(List(R4)),
			Comment("Start of multiplication", 4),
			Comment("Start of identifier", 4),
			Mov(R4, R5, noCondition),
			Comment("End of identifier", 4),
			Push(List(R4)),
			Comment("Start of number", 4),
			Mov(R4, ImmVal(3), noCondition),
			Push(List(R4)),
			Pop(List(R6, R7)),
			SmullInstr(R4, R8, R6, R7),
			CmpInstr(R8, RegShift(R4, ShiftRight(31))),
			BInstr("_errOverflow", NEcond, true),
			Comment("End of multiplication expression", 4),
			Pop(List(R7)),
			AddInstr(R4, R4, R7, true),
			BInstr("_errOverflow", VScond, true),
			Comment("End of addition expression", 4),
			Comment("Return Logic", 4),
			Mov(R0, R4, noCondition),
			Mov(SP, FP, noCondition),
			Pop(List(R4, R5, R6, R7, R8, R9, R10)),
			Pop(List(FP, PC)),
			Pop(List(R0, R1, R2, R3)),
			Command("ltorg", 4),
			NewLine(),
			Comment("Start of function", 4),
			Label("wacc_g_1"),
			Push(List(FP, LR)),
			Push(List(R4, R5, R6, R7, R8, R9, R10)),
			Mov(FP, SP, noCondition),
			SubInstr(SP, SP, ImmVal(0), false),
			Mov(R5, R0, noCondition),
			Push(List(R0, R1, R2, R3)),
			Comment("Start of return", 4),
			Comment("Start of multiplication", 4),
			Comment("Start of identifier", 4),
			Mov(R4, R5, noCondition),
			Comment("End of identifier", 4),
			Push(List(R4)),
			Comment("Start of addition expression", 4),
			Comment("Start of number", 4),
			Mov(R4, ImmVal(1), noCondition),
			Push(List(R4)),
			Comment("Start of number", 4),
			Mov(R4, ImmVal(3), noCondition),
			Pop(List(R6)),
			AddInstr(R4, R4, R6, true),
			BInstr("_errOverflow", VScond, true),
			Comment("End of addition expression", 4),
			Push(List(R4)),
			Pop(List(R6, R7)),
			SmullInstr(R4, R8, R6, R7),
			CmpInstr(R8, RegShift(R4, ShiftRight(31))),
			BInstr("_errOverflow", NEcond, true),
			Comment("End of multiplication expression", 4),
			Comment("Return Logic", 4),
			Mov(R0, R4, noCondition),
			Mov(SP, FP, noCondition),
			Pop(List(R4, R5, R6, R7, R8, R9, R10)),
			Pop(List(FP, PC)),
			Pop(List(R0, R1, R2, R3)),
			Command("ltorg", 4)
		)
    
    val (mainOutput, predefOutput, _) = controlFlowOptimise(program, ListBuffer(), PredefinedFunctions.getPredefinedFunctions)

    mainOutput should be (ListBuffer(
      Command("align 4",0), 
      Command("text",0), 
      Command("global main",0), 
      Label("main"), 
      Push(List(FP, LR)), 
      Mov(FP,SP,noCondition), 
      Mov(R5,R5,noCondition), 
      Mov(R5,ImmVal(5),noCondition), 
      Push(List(R1, R2, R3)), 
      Mov(R6,R5,noCondition), 
      Mov(R0,R6,noCondition), 
      BInstr("wacc_f_0",noCondition,true), 
      Pop(List(R1, R2, R3)), 
      Mov(R4,R0,noCondition), 
      Mov(R5,R4,noCondition), 
      Mov(R4,R5,noCondition), 
      Mov(R0,R4,noCondition), 
      BInstr("_printi",noCondition,true), 
      BInstr("_println",noCondition,true), 
      Mov(R0,ImmVal(0),noCondition), 
      Pop(List(FP, PC)), 
      NewLine(), 
      Label("wacc_f_0"), 
      Push(List(FP, LR)), 
      Push(List(R4, R5, R6, R7, R8, R9, R10)), 
      Mov(FP,SP,noCondition), 
      SubInstr(SP,SP,ImmVal(0),false), 
      Mov(R5,R0,noCondition), 
      Push(List(R0, R1, R2, R3)), 
      Mov(R4,ImmVal(1),noCondition), 
      Push(List(R4)), 
      Mov(R4,R5,noCondition), 
      Push(List(R4)), 
      Mov(R4,ImmVal(3),noCondition), 
      Push(List(R4)), 
      Pop(List(R6, R7)), 
      SmullInstr(R4,R8,R6,R7), 
      CmpInstr(R8,RegShift(R4,ShiftRight(31))), 
      BInstr("_errOverflow",NEcond,true), 
      Pop(List(R7)), 
      AddInstr(R4,R4,R7,true), 
      BInstr("_errOverflow",VScond,true), 
      Mov(R0,R4,noCondition), 
      Mov(SP,FP,noCondition), 
      Pop(List(R4, R5, R6, R7, R8, R9, R10)), 
      Pop(List(FP, PC)), 
      Pop(List(R0, R1, R2, R3)), 
      Command("ltorg",4)))

    predefOutput should be (ListBuffer(
      NewLine(), 
      AscizInstr(".L._prints_str0",StringLiteral("%.*s")), 
      Command("align 4",0), 
      Label("_prints"), 
      Push(List(FP, LR)), 
      Mov(FP,SP,noCondition), 
      BicInstr(SP,SP,ImmVal(7)), 
      Mov(R2,R0,noCondition), 
      Ldr(R1,Addr(R0,ImmVal(-4))), 
      AdrInstr(R0,".L._prints_str0"), 
      BInstr("printf",noCondition,true), 
      Mov(R0,ImmVal(0),noCondition), 
      BInstr("fflush",noCondition,true), 
      Mov(SP,FP,noCondition), 
      Pop(List(FP, PC)), 
      NewLine(), 
      AscizInstr(".L._errOverflow_str0",ErrorMessage(IntegerOverflowUnderflowErr)), 
      Command("align 4",0), 
      Label("_errOverflow"), 
      Push(List(FP, LR)), 
      Mov(FP,SP,noCondition), 
      BicInstr(SP,SP,ImmVal(7)), 
      AdrInstr(R0,".L._errOverflow_str0"), 
      BInstr("_prints",noCondition,true), 
      Mov(R0,ImmVal(255),noCondition), 
      BInstr("exit",noCondition,true), 
      Mov(SP,FP,noCondition), 
      Pop(List(FP, PC)), 
      NewLine(), 
      AscizInstr(".L._println_str0",StringLiteral("")), 
      Command("align 4",0), 
      Label("_println"), 
      Push(List(FP, LR)), 
      Mov(FP,SP,noCondition), 
      BicInstr(SP,SP,ImmVal(7)), 
      AdrInstr(R0,".L._println_str0"), 
      BInstr("puts",noCondition,true), 
      Mov(R0,ImmVal(0),noCondition), 
      BInstr("fflush",noCondition,true), 
      Mov(SP,FP,noCondition), 
      Pop(List(FP, PC)), 
      NewLine(), 
      AscizInstr(".L._printi_str0",StringLiteral("%d")), 
      Command("align 4",0), 
      Label("_printi"), 
      Push(List(FP, LR)), 
      Mov(FP,SP,noCondition), 
      BicInstr(SP,SP,ImmVal(7)), 
      Mov(R1,R0,noCondition), 
      AdrInstr(R0,".L._printi_str0"), 
      BInstr("printf",noCondition,true), 
      Mov(R0,ImmVal(0),noCondition), 
      BInstr("fflush",noCondition,true), 
      Mov(SP,FP,noCondition), 
      Pop(List(FP, PC))))
  }
}
