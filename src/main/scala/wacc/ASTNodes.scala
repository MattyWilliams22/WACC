package wacc

import wacc.Main.{SEMANTIC_ERR_CODE, SUCCESS_CODE}
import scala.util.chaining

object ASTNodes {

  var currentSymbolTable: SymbolTable = new SymbolTable(None)

  // Prints an error message and exits if valid is false
  def checkValid(valid: Boolean, errorMessage: String, node: ASTNode): Unit = {
    if (!valid) {
      println("Semantic Error in ASTNode: " + node)
      println(s"  $errorMessage")
      System.exit(SEMANTIC_ERR_CODE)
    }
  }

  sealed trait ASTNode

  case class Program(funcs: List[Function], statement: Statement) extends ASTNode {
    val symbolTable: SymbolTable = new SymbolTable(None)

    // Checks the semantics of a program
    def check(): Unit = {
      var valid: Boolean = true
      currentSymbolTable = symbolTable

      // Checks semantics of each function in the program
      for (func <- funcs) {
        valid = valid && func.check()
        checkValid(valid, "Invalid function", func)
      }

      // Checks that there are no return statements in the main body of the program
      checkReturns(statement)

      // Checks the main body of the program
      valid = valid && statement.check()
      checkValid(valid, "Main body of program is invalid", statement)

      System.exit(SUCCESS_CODE)
    }
    
    // Checks if the statement contains a return statement
    // If it does, exit with semantic error code
    def checkReturns(stmt: Statement): Unit = {
      stmt match {
        case stmts: Statements =>
          // Check each statement in the list of statements
          for (stat <- stmts.stmts) {
            // If the statement is Return(_), exit with the semantic error code
            // Else, If the statement contains more statements, recursively check these
            stat match {
              case Return(_) => checkValid(false, "Return statement in main body", stat)
              case While(_,body) => checkReturns(body)
              case If(_,thenS,elseS) => {
                checkReturns(thenS)
                checkReturns(elseS)
              }
              case Scope(body) => checkReturns(body)
              case _ =>
            }
          }
        // If the single statement is Return(_), exit with the semantic error code
        case Return(_) => checkValid(false, "Return statement in main body", stmt)
        case _ =>
      }
    }
  }

  case class Function(_type: Type, ident: Ident, param_list: List[Param], body: Statement) extends ASTNode {
    val argSymbolTable: SymbolTable = new SymbolTable(None)
    val bodySymbolTable: SymbolTable = new SymbolTable(Option(argSymbolTable))

    // Checks the semantics of a function
    def check(): Boolean = {
      var valid: Boolean = true
      val tempSymbolTable: SymbolTable = currentSymbolTable
      currentSymbolTable = bodySymbolTable

      // Check the body of the function
      valid = valid && body.check()
      checkValid(valid, "Invalid function body", body)

      // Check that all the return statements have type _type
      val retTs = getReturns(body)
      val retT = retTs.head
      for (thisRetT <- retTs) {
        valid = valid && thisRetT == retT
        checkValid(valid, "Return values have different types", body)
      }
      valid = valid && retT == _type
      checkValid(valid, "Return values have different type to function type", this)

      currentSymbolTable = tempSymbolTable
      valid
    }

    // Returns a list of types of every return in the statement
    def getReturns(stmt: Statement): List[Type] = {
      stmt match {
        case stmts: Statements =>
          var returnTypes: List[Type] = List.empty
          for (stat <- stmts.stmts) {
            // Recursively check the return type of every statement
            val returnType = getReturns(stat)
            if (returnType.nonEmpty && returnType.head != BaseT("ERROR")) {
              // Return early if the list is empty
              return returnType
            }
            // Concatenate the types
            returnTypes = returnTypes ++ returnType
          }
          returnTypes

        // If a return statement is found, return a singleton list of its type
        case r: Return => List(r.getType())
        // Recursively check statements containing more statements
        case While(_, wbody) => getReturns(wbody)
        case If(_, thenS, elseS) => {
          val tthen = getReturns(thenS)
          val telse = getReturns(elseS)
          var ts: List[Type] = List.empty
          // Combine the valid types from the then and else cases
          if (tthen.nonEmpty && tthen.head != BaseT("ERROR")) {
            ts = ts ++ tthen
          } 
          if (telse.nonEmpty && telse.head != BaseT("ERROR")) {
            ts = ts ++ telse
          } 
          ts
        }
        case Scope(sbody) => getReturns(sbody)
        // For any other statement, return a singleton of the erroneous type
        case _ => List(BaseT("ERROR"))
      }
    }
  }

  case class Param(_type: Type, ident: Ident) extends ASTNode {
    // Semantically check a parameter
    def check(): Boolean = {
      _type == ident.getType()
    }

    // Get the type of a parameter
    def getType(): Type = {
      _type.getType()
    }
  }

  sealed trait Statement extends ASTNode {
    def check(): Boolean
  }

  case class Skip() extends Statement {
    // Semantically check the skip statement,
    // which is always semantically valid
    def check(): Boolean = true
  }

  case class Declare(_type: Type, ident: Ident, value: RValue) extends Statement {
    // Semantically check a declare statement
    def check(): Boolean = {
      currentSymbolTable.generateSymbolTable(this)

      // Check the value on the right of the declaration
      var valid: Boolean = value.check()
      checkValid(valid, "Invalid Rvalue", value)

      // Check that the type of the variable and the type of the value are complementary
      val tValue = value.getType()
      val isPair = _type match {
        case PairT(_, _) => true
        case _ => false
      }
      val isEmptyArrayLiteral = tValue match {
        case ArrayT(BaseT("Empty"), _) => true
        case _ => false
      }
      val isPairOfNulls = tValue match {
        case PairT(PairNull(),PairNull()) => true
        case _ => false
      }
      valid = valid &&
        (_type == tValue ||
          isPair && (tValue == PairLiter("null") || tValue == PairNull()) ||
          isEmptyArrayLiteral && _type.isInstanceOf[ArrayT] ||
          _type == BaseT("string") && tValue == ArrayT(BaseT("char"), 1) ||
          isPairOfNulls)
      checkValid(valid, "Types of variable and rvalue do not match", this)
      valid
    }
  }

  case class Assign(lvalue: LValue, rvalue: RValue) extends Statement {
    // Semantically check an assignment statement
    def check(): Boolean = {
      // Check that the lvalue and rvalue are semantically valid
      var valid: Boolean = lvalue.check() 
      checkValid(valid, "Invalid lvalue", lvalue)
      valid = valid && rvalue.check()
      checkValid(valid, "Invalid rvalue", rvalue)

      // Check that the types of lvalue and rvalue are complementary
      val tlvalue = lvalue.getType()
      var trvalue = rvalue.getType()
      trvalue = trvalue match {
        case ArrayT(BaseT("Empty"),0) => PairNull()
        case _ => trvalue
      }
      val isPair = tlvalue match {
        case PairT(_, _) => true
        case _ => false
      }
      valid = valid && (tlvalue == trvalue ||
        isPair && (trvalue == PairLiter("null") ||
          trvalue == PairNull()))
      lvalue match {
        case ident: Ident => {
          if (ident.isFunction()) {
            valid = false
          }
        }
        case _ =>
      }
      checkValid(valid, "Types of lvalue and rvalue do not match", this)
      valid
    }
  }

  case class Read(lvalue: LValue) extends Statement {
    // Semantically check a read statement
    def check(): Boolean = {
      // Semantically check the lvalue
      var valid = lvalue.check()
      checkValid(valid, "Invalid lvalue", lvalue)

      // Ensure lvalue has type int or type char
      val tlvalue = lvalue.getType()
      valid = valid && (tlvalue == BaseT("int") || tlvalue == BaseT("char"))
      checkValid(valid, "Lvalue must have type int or type char", lvalue)
      valid
    }
  }

  case class If(cond: Expr, thenS: Statement, elseS: Statement) extends Statement {
    val thenSymbolTable: SymbolTable = new SymbolTable(None)
    val elseSymbolTable: SymbolTable = new SymbolTable(None)

    // Semantically check an if-then-else statement
    def check(): Boolean = {
      currentSymbolTable.generateSymbolTable(this)

      // Check that the condition is semantically valid and has type bool
      var valid = true;
      valid = valid && cond.check() 
      checkValid(valid, "Invalid condition of if statement", cond)
      valid = valid && cond.getType() == BaseT("bool")
      checkValid(valid, "Condition must have type bool", cond)
    
      val tempSymbolTable = currentSymbolTable

      // Check that the body of the then block is valid
      currentSymbolTable = thenSymbolTable
      valid = thenS.check()
      checkValid(valid, "Body of then case is invalid", thenS)

      // Check that the body of the else block is valid
      currentSymbolTable = elseSymbolTable
      valid = elseS.check()
      checkValid(valid, "Body of else case is invalid", elseS)

      currentSymbolTable = tempSymbolTable
      valid
    }
  }

  case class While(cond: Expr, body: Statement) extends Statement {
    val symbolTable: SymbolTable = new SymbolTable(None)

    // Semantically check while statements
    def check(): Boolean = {
      currentSymbolTable.generateSymbolTable(this)
      val tempSymbolTable: SymbolTable = currentSymbolTable
      currentSymbolTable = symbolTable

      // Check that the condition is semantically valid and has type bool
      var valid = cond.check() 
      checkValid(valid, "Invalid condition of while loop", cond)

      valid = valid && body.check() 
      checkValid(valid, "Invalid body of while loop", body)

      valid = valid && cond.getType() == BaseT("bool")
      checkValid(valid, "Condition must have type bool", cond)

      currentSymbolTable = tempSymbolTable
      valid
    }
  }

  case class Scope(body: Statement) extends Statement {
    val symbolTable: SymbolTable = new SymbolTable(None)

    // Semantically check a scope statement
    def check(): Boolean = {
      currentSymbolTable.generateSymbolTable(this)
      val tempSymbolTable: SymbolTable = currentSymbolTable
      currentSymbolTable = symbolTable

      // Check that the body of the scope is semantically valid
      var valid = body.check()
      checkValid(valid, "Invalid body of scope", body)

      currentSymbolTable = tempSymbolTable
      valid
    }
  }

  case class Statements(stmts: List[Statement]) extends Statement {
    // Semantically check a list of statements
    def check(): Boolean = {
      var valid: Boolean = true
      // Check that each statement in the list is semantically valid
      for (stat <- stmts) {
        valid = valid && stat.check()
        checkValid(valid, "Invalid statement", stat)
      }
      valid
    }
  }

  case class Free(exp: Expr) extends Statement {
    // Semantically check a free statement
    def check(): Boolean = {
      // Check that the expression is semantically valid and has type pair or array
      val t: Type = exp.getType()
      var valid = exp.check()
      checkValid(valid, "Invalid expression", exp)
      valid = valid && (t match {
        case PairT(_, _) => true
        case ArrayT(_, n) if n > 0 => true
        case _ => false
      })
      checkValid(valid, "Free can only be used on arrays and pairs", exp)
      valid
    }
  }

  case class Return(exp: Expr) extends Statement {
    // Semantically check a return statement
    def check(): Boolean = {
      // Check that the expression is semantically valid
      exp.check()
    }

    // Get the type of the return statement
    def getType(): Type = {
      // Get the type of the expression
      exp.getType()
    }
  }

  case class Exit(exp: Expr) extends Statement {
    // Semantically check an exit statement
    def check(): Boolean = {
      // Check that the expression is semantically valid and has type int
      var valid = exp.check() 
      checkValid(valid, "Invalid expression", exp)
      valid = valid && exp.getType() == BaseT("int")
      checkValid(valid, "Expression must have type int", exp)
      valid
    }
  }

  case class Print(exp: Expr) extends Statement {
    // Semantically check a print statement
    def check(): Boolean = {
      // Check that the expression is semantically valid
      exp.check()
    }
  }
  case class Println(exp: Expr) extends Statement {
    // Semantically check a println statement
    def check(): Boolean = {
      // Check that the expression is semantically valid
      exp.check()
    }
  }


  sealed trait Type extends ASTNode {
    def check(): Boolean

    def getType(): Type
  }

  sealed trait PairElemT extends Type {
    def check(): Boolean

    def getType(): Type
  }

  case class BaseT(str: String) extends Type with PairElemT {
    // Check if the type is valid
    def check(): Boolean = {
      // Check if the type is int, bool, char or string
      val valid = str == "int" || str == "bool" || str == "char" || str == "string"
      checkValid(valid, "Base type must be int, bool, char or string", this)
      valid
    }

    def getType(): Type = {
      // Return the type
      this
    }
  }

  case class ArrayT(_type: Type, dim: Int) extends Type with PairElemT {
    // Check if the type is valid
    def check(): Boolean = {
      // Check if the type is an array
      _type.check()
    }

    // Get the type of the array
    def getType(): Type = {
      // Return the type
      this
    }
  }

  case class PairT(pet1: PairElemT, pet2: PairElemT) extends Type {
    // Semantically check if a pair is valid
    def check(): Boolean = {
      // Check that each element of the pair is valid
      var valid = pet1.check() 
      checkValid(valid, "Invalid pair elem", pet1)
      valid = valid && pet2.check()
      checkValid(valid, "Invalid pair elem", pet2)
      valid
    }

    // Get the type of the pair
    def getType(): Type = {
      // Return the type
      this
    }
  }

  case class PairNull() extends PairElemT {
    // Semantically check if a pair null is valid
    def check(): Boolean = {
      true
    }

    // Get the type of a pair null
    def getType(): Type = {
      this
    }
  }

  sealed trait LValue extends ASTNode {
    def check(): Boolean

    def getType(): Type
  }

  sealed trait RValue extends ASTNode {
    def check(): Boolean

    def getType(): Type
  }

  case class ArrayLiter(elems: List[Expr]) extends RValue {
    // Semantically check an array literal
    def check(): Boolean = {
      var valid: Boolean = true
      // Check that each element in the array is semantically valid
      for (elem <- elems) {
        valid = valid && elem.check()
        checkValid(valid, "Invalid array elem", elem)
      }

      def isStringOrCharArray(t: Type): Boolean = t match {
        case BaseT("string") => true
        case ArrayT(BaseT("char"), _) => true
        case _ => false
      }

      // Check that the elements in the array are all of the same type
      valid = elems match {
        case head :: tail =>
          val firstType = head.getType()
          firstType match {
            case BaseT("string") | ArrayT(BaseT("char"), _) =>
              tail.forall(elem => isStringOrCharArray(elem.getType()))
            case _ => tail.forall(elem => elem.getType() == firstType)
          }
        case _ => true
      }
      checkValid(valid, "Invalid mixing of types in array literal", this)
      valid
    }

    // Get the type of the array literal
    def getType(): Type = {
      // If the array is empty, return the type of the first element
      if (elems.isEmpty) {
        ArrayT(BaseT("Empty"), 0)
      } else {
        // If the array contains a string, return the type of the string
        if (elems.exists(elem => elem.getType() == BaseT("string"))) {
          ArrayT(BaseT("string"), 1)
        } else {
          // If there is no string, return the type of the first element
          elems.head.getType() match {
            case ArrayT(t, n) => ArrayT(t, n + 1)
            case t => ArrayT(t, 1)
          }
        }
      }
    }
  }

  case class NewPair(exp1: Expr, exp2: Expr) extends RValue {
    // Semantically check a new pair statement
    def check(): Boolean = {
      // Check that the expressions are semantically valid
      var valid = exp1.check()
      checkValid(valid, "Invalid expression", exp1)
      valid = valid && exp2.check()
      checkValid(valid, "Invalid expression", exp2)
      valid
    }

    // Get the type of the new pair statement
    def getType(): Type = {
      // Get the types of the expressions
      val type1: Type = exp1.getType()
      val type2: Type = exp2.getType()

      // Convert types to PairElemT
      val vtype1 = if (type1.isInstanceOf[PairElemT]) {
        type1.asInstanceOf[PairElemT]
      } else if (type1.isInstanceOf[PairLiter]) {
        PairNull()
      } else if (type1.isInstanceOf[PairT]) {
        PairNull()
      } else {
        BaseT("ERROR")
      }
      val vtype2 = if (type2.isInstanceOf[PairElemT]) {
        type2.asInstanceOf[PairElemT]
      } else if (type2.isInstanceOf[PairLiter]) {
        PairNull()
      } else if (type2.isInstanceOf[PairT]) {
        PairNull()
      } else {
        BaseT("ERROR")
      }

      // Return the type of the pair
      if (vtype1 != BaseT("ERROR") && vtype2 != BaseT("ERROR")) {
        PairT(vtype1, vtype2)
      } else {
        BaseT("ERROR")
      }
    }
  }

  case class PairElem(func: String, lvalue: LValue) extends RValue with LValue {
    // Semantically check a pair element
    def check(): Boolean = {
      lvalue.check()
    }

    // Get the type of the pair element
    def getType(): Type = {
      // Get the type of the parent of the pair element
      def parentT: Type = lvalue.getType()
      // If the parent is a pair, return the type of the child
      parentT match {
        case t1: PairT =>
          def pairT: PairT = t1

          var childT: Type = func match {
            case "fst" => pairT.pet1
            case "snd" => pairT.pet2
          }
          if (childT == PairNull()) {
            childT = lvalue match {
              case Ident(str) => {
                currentSymbolTable.lookupAllVariables(str) match {
                  case Some(Declare(t, _, _)) => t
                  case _ => childT
                }
              }
              case _ => childT
            }
          }
          childT
        case _ =>
          // If the parent is not a pair, return an error
          BaseT("ERROR")
      }
    }
  }

  case class Call(funcName: Ident, args: List[Expr]) extends RValue {
    // Semantically check a function call
    def check(): Boolean = {
      var valid: Boolean = true

      // Check that the function name is semantically valid
      valid = valid && funcName.check()
      checkValid(valid, "Function does not exist in scope", funcName)

      // Check that the arguments are semantically valid and have the correct types
      val funcForm = funcName.getNode()
      funcForm match {
        case Function(t, ident, params, body) => {
          valid = valid && args.length == params.length
          checkValid(valid, "Invalid number of arguments", Call(funcName, args))
          for (i <- 0 to params.length - 1) {
            args(i).check()
            checkValid(valid, "Invalid argument", args(i))
            valid = valid && args(i).getType() == params(i).getType()
            checkValid(valid, "Invalid argument type", args(i))
          }
        }
        case _ =>
      }
      valid
    }

    // Get the type of the function call
    def getType(): Type = {
      funcName.getType()
    }
  }

  sealed trait Expr extends RValue {
    def check(): Boolean

    def getType(): Type
  }

  case class Mul(exp1: Expr, exp2: Expr) extends Expr {
    // Semantically check a multiplication expression
    def check(): Boolean = {
      // Check that the expressions are semantically valid and have type int
      var valid = exp1.check()
      checkValid(valid, "Invalid expression", exp1)
      valid = valid && exp2.check()
      checkValid(valid, "Invalid expression", exp2)
      valid = valid && exp1.getType() == BaseT("int")
      checkValid(valid, "Expression must have type int", exp1)
      valid = valid && exp2.getType() == BaseT("int")
      checkValid(valid, "Expression must have type int", exp2)
      valid
    }

    // Get the type of the multiplication expression
    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Div(exp1: Expr, exp2: Expr) extends Expr {
    // Semantically check a division expression
    def check(): Boolean = {
      // Check that the expressions are semantically valid and have type int
      var valid = exp1.check()
      checkValid(valid, "Invalid expression", exp1)
      valid = valid && exp2.check()
      checkValid(valid, "Invalid expression", exp2)
      valid = valid && exp1.getType() == BaseT("int")
      checkValid(valid, "Expression must have type int", exp1)
      valid = valid && exp2.getType() == BaseT("int")
      checkValid(valid, "Expression must have type int", exp2)
      valid
    }

    // Get the type of the division expression
    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Mod(exp1: Expr, exp2: Expr) extends Expr {
    // Semantically check a modulo expression
    def check(): Boolean = {
      // Check that the expressions are semantically valid and have type int
      var valid = exp1.check()
      checkValid(valid, "Invalid expression", exp1)
      valid = valid && exp2.check()
      checkValid(valid, "Invalid expression", exp2)
      valid = valid && exp1.getType() == BaseT("int")
      checkValid(valid, "Expression must have type int", exp1)
      valid = valid && exp2.getType() == BaseT("int")
      checkValid(valid, "Expression must have type int", exp2)
      valid
    }

    // Get the type of the modulo expression
    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Add(exp1: Expr, exp2: Expr) extends Expr {
    // Semantically check an addition expression
    def check(): Boolean = {
      // Check that the expressions are semantically valid and have type int
      var valid = exp1.check()
      checkValid(valid, "Invalid expression", exp1)
      valid = valid && exp2.check()
      checkValid(valid, "Invalid expression", exp2)
      valid = valid && exp1.getType() == BaseT("int")
      checkValid(valid, "Expression must have type int", exp1)
      valid = valid && exp2.getType() == BaseT("int")
      checkValid(valid, "Expression must have type int", exp2)
      valid
    }

    // Get the type of the addition expression
    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Sub(exp1: Expr, exp2: Expr) extends Expr {
    // Semantically check a subtraction expression
    def check(): Boolean = {
      // Check that the expressions are semantically valid and have type int
      var valid = exp1.check()
      checkValid(valid, "Invalid expression", exp1)
      valid = valid && exp2.check()
      checkValid(valid, "Invalid expression", exp2)
      valid = valid && exp1.getType() == BaseT("int")
      checkValid(valid, "Expression must have type int", exp1)
      valid = valid && exp2.getType() == BaseT("int")
      checkValid(valid, "Expression must have type int", exp2)
      valid
    }

    // Get the type of the subtraction expression
    def getType(): Type = {
      BaseT("int")
    }
  }

  case class GT(exp1: Expr, exp2: Expr) extends Expr {
    // Semantically check a greater than expression
    def check(): Boolean = {
      // Check that the expressions are semantically valid and have type int or char
      val type1: Type = exp1.getType()
      val type2: Type = exp2.getType()

      var valid = exp1.check()
      checkValid(valid, "Invalid expression", exp1)
      valid = valid && exp2.check()
      checkValid(valid, "Invalid expression", exp2)
      valid = valid && ((type1 == BaseT("int") && type2 == BaseT("int")) ||
        (type1 == BaseT("char") && type2 == BaseT("char")))
      checkValid(valid, "Both expressions must have type int or char", this)
      valid
    }

    // Get the type of the greater than expression
    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class GTEQ(exp1: Expr, exp2: Expr) extends Expr {
    // Semantically check a greater than or equal to expression
    def check(): Boolean = {
      // Check that the expressions are semantically valid and have type int or char
      val type1: Type = exp1.getType()
      val type2: Type = exp2.getType()

      var valid = exp1.check()
      checkValid(valid, "Invalid expression", exp1)
      valid = valid && exp2.check()
      checkValid(valid, "Invalid expression", exp2)
      valid = valid && ((type1 == BaseT("int") && type2 == BaseT("int")) ||
        (type1 == BaseT("char") && type2 == BaseT("char")))
      checkValid(valid, "Both expressions must have type int or char", this)
      valid
    }

    // Get the type of the greater than or equal to expression
    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class LT(exp1: Expr, exp2: Expr) extends Expr {
    // Semantically check a less than expression
    def check(): Boolean = {
      // Check that the expressions are semantically valid and have type int or char
      val type1: Type = exp1.getType()
      val type2: Type = exp2.getType()

      var valid = exp1.check()
      checkValid(valid, "Invalid expression", exp1)
      valid = valid && exp2.check()
      checkValid(valid, "Invalid expression", exp2)
      valid = valid && ((type1 == BaseT("int") && type2 == BaseT("int")) ||
        (type1 == BaseT("char") && type2 == BaseT("char")))
      checkValid(valid, "Both expressions must have type int or char", this)
      valid
    }

    // Get the type of the less than expression
    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class LTEQ(exp1: Expr, exp2: Expr) extends Expr {
    // Semantically check a less than or equal to expression
    def check(): Boolean = {
      // Check that the expressions are semantically valid and have type int or char
      val type1: Type = exp1.getType()
      val type2: Type = exp2.getType()

      var valid = exp1.check()
      checkValid(valid, "Invalid expression", exp1)
      valid = valid && exp2.check()
      checkValid(valid, "Invalid expression", exp2)
      valid = valid && ((type1 == BaseT("int") && type2 == BaseT("int")) ||
        (type1 == BaseT("char") && type2 == BaseT("char")))
      checkValid(valid, "Both expressions must have type int or char", this)
      valid
    }

    // Get the type of the less than or equal to expression
    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class EQ(exp1: Expr, exp2: Expr) extends Expr {
    // Semantically check an equal to expression
    def check(): Boolean = {
      // Check that the expressions are semantically valid
      var valid = exp1.check()
      checkValid(valid, "Invalid expression", exp1)
      valid = valid && exp2.check()
      checkValid(valid, "Invalid expression", exp2)
      valid
    }

    // Get the type of the equal to expression
    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class NEQ(exp1: Expr, exp2: Expr) extends Expr {
    // Semantically check a not equal to expression
    def check(): Boolean = {
      // Check that the expressions are semantically valid
      var valid = exp1.check()
      checkValid(valid, "Invalid expression", exp1)
      valid = valid && exp2.check()
      checkValid(valid, "Invalid expression", exp2)
      valid
    }

    // Get the type of the not equal to expression
    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class And(exp1: Expr, exp2: Expr) extends Expr {
    // Semantically check an and expression
    def check(): Boolean = {
      // Check that the expressions are semantically valid and have type bool
      var valid = exp1.check()
      checkValid(valid, "Invalid expression", exp1)
      valid = valid && exp2.check()
      checkValid(valid, "Invalid expression", exp2)
      valid = valid && exp1.getType() == BaseT("bool")
      checkValid(valid, "Expression must have type bool", exp1)
      valid = valid && exp2.getType() == BaseT("bool")
      checkValid(valid, "Expression must have type bool", exp2)
      valid
    }

    // Get the type of the and expression
    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class Or(exp1: Expr, exp2: Expr) extends Expr {
    // Semantically check an or expression
    def check(): Boolean = {
      // Check that the expressions are semantically valid and have type bool
      var valid = exp1.check()
      checkValid(valid, "Invalid expression", exp1)
      valid = valid && exp2.check()
      checkValid(valid, "Invalid expression", exp2)
      valid = valid && exp1.getType() == BaseT("bool")
      checkValid(valid, "Expression must have type bool", exp1)
      valid = valid && exp2.getType() == BaseT("bool")
      checkValid(valid, "Expression must have type bool", exp2)
      valid
    }

    // Get the type of the or expression
    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class Not(exp: Expr) extends Expr {
    // Semantically check a not expression
    def check(): Boolean = {
      // Check that the expression is semantically valid and has type bool
      var valid = exp.check()
      checkValid(valid, "Invalid expression", exp)
      valid = valid && exp.getType() == BaseT("bool")
      checkValid(valid, "Expression must have type bool", exp)
      valid
    }

    // Get the type of the not expression
    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class Neg(exp: Expr) extends Expr {
    // Semantically check a negation expression
    def check(): Boolean = {
      // Check that the expression is semantically valid and has type int
      var valid = exp.check()
      checkValid(valid, "Invalid expression", exp)
      valid = valid && exp.getType() == BaseT("int")
      checkValid(valid, "Expression must have type int", exp)
      valid
    }

    // Get the type of the negation expression
    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Len(exp: Expr) extends Expr {
    // Semantically check a length expression
    def check(): Boolean = {
      // Check that the expression is semantically valid and has type array
      val arrT: Type = exp.getType()

      var valid = exp.check()
      checkValid(valid, "Invalid expression", exp)
      valid = valid && (arrT match {
        case ArrayT(_, n) if n > 0 => true
        case _ => false
      })
      checkValid(valid, "Expression must have type array", exp)
      valid
    }

    // Get the type of the length expression
    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Ord(exp: Expr) extends Expr {
    // Semantically check an ord expression
    def check(): Boolean = {
      // Check that the expression is semantically valid and has type char
      var valid = exp.check()
      checkValid(valid, "Invalid expression", exp)
      valid = valid && exp.getType() == BaseT("char")
      checkValid(valid, "Expression must have type char", exp)
      valid
    }

    // Get the type of the ord expression
    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Chr(exp: Expr) extends Expr {
    // Semantically check a chr expression
    def check(): Boolean = {
      // Check that the expression is semantically valid and has type int
      var valid = exp.check()
      checkValid(valid, "Invalid expression", exp)
      valid = valid && exp.getType() == BaseT("int")
      checkValid(valid, "Expression must have type int", exp)
      valid
    }

    // Get the type of the chr expression
    def getType(): Type = {
      BaseT("char")
    }
  }

  sealed trait Atom extends Expr

  case class Num(value: Int) extends Atom {
    // Semantically check a number
    def check(): Boolean = {
      true
    }

    // Get the type of the number
    def getType(): Type = {
      BaseT("int")
    }
  }

  case class Bool(bool: String) extends Atom {
    // Semantically check a boolean
    def check(): Boolean = {
      // Check that the boolean is either true or false
      var valid = bool == "true" || bool == "false"
      checkValid(valid, "Boolean must be true or false", this)
      valid
    }

    // Get the type of the boolean
    def getType(): Type = {
      BaseT("bool")
    }
  }

  case class Ch(chr: Char) extends Atom {
    // Semantically check a character
    def check(): Boolean = {
      true
    }

    // Get the type of the character
    def getType(): Type = {
      BaseT("char")
    }
  }

  case class Str(str: String) extends Atom {
    // Semantically check a string
    def check(): Boolean = {
      true
    }

    // Get the type of the string
    def getType(): Type = {
      BaseT("string")
    }
  }

  case class PairLiter(str: String) extends Atom with Type {
    // Semantically check a pair literal
    def check(): Boolean = {
      true
    }

    // Get the type of the pair literal
    def getType(): Type = {
      PairLiter(str)
    }
  }

  case class Ident(str: String) extends Atom with LValue {
    // Semantically check an identifier
    def check(): Boolean = {
      // Check that the identifier is in the symbol table
      var valid = (currentSymbolTable.lookupAllVariables(str).isDefined ||
        currentSymbolTable.lookupAllFunctions(str).isDefined)
      checkValid(valid, "Identifier does not exist in scope", this)
      valid
    }

    // Get the type of the identifier
    def getType(): Type = {
      // Search the symbol table for the type of the identifier
      currentSymbolTable.lookupAllVariables(str) match {
        case Some(x) => x match {
          case param: Param =>
            param._type
          case declare: Declare =>
            declare._type
          case _ =>
            BaseT("ERROR")
        }
        case None => currentSymbolTable.lookupAllFunctions(str) match {
          case Some(x) => x._type
          case None => BaseT("ERROR")
        }
      }
    }

    // Get the node of the identifier
    def getNode(): ASTNode = {
      currentSymbolTable.lookupAllVariables(str) match {
        case Some(x) => x
        case None => currentSymbolTable.lookupAllFunctions(str) match {
          case Some(x) => x
          case None => BaseT("ERROR")
        }
      }
    }

    // Check if the identifier is a function
    def isFunction(): Boolean = {
      currentSymbolTable.lookupAllFunctions(str).isDefined
    }
  }

  case class ArrayElem(ident: Ident, indices: List[Expr]) extends Atom with LValue {
    // Semantically check an array element
    def check(): Boolean = {
      var valid: Boolean = true
      // Check that the identifier is semantically valid
      valid = valid && ident.check()
      checkValid(valid, "Invalid identifier", ident)
      // Check that the indices are semantically valid and have type int
      for (index <- indices) {
        valid = valid && (index.getType() == BaseT("int"))
        checkValid(valid, "Index must have type int", index)
      }
      val tident = ident.getType()
      tident match {
        case t: ArrayT =>
          // Check that the number of indices is less than or equal to the dimensions of the array
          valid = valid && t.dim >= indices.length
          checkValid(valid, "Invalid number of indices", this)
        case _ =>
          checkValid(false, "Identifier must be have type array", this)
      }
      valid
    }

    // Get the type of the array element
    def getType(): Type = {
      ident.getType() match {
        // Reduce the dimensions of the array by the number of indices
        case ArrayT(t, n) if n > indices.length => ArrayT(t, n - indices.length)
        case ArrayT(t, n) if n == indices.length => t
        case _ => BaseT("ERROR")
      }
    }
  }
}
