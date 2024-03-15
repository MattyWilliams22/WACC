package wacc

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import parsley.{Failure, Result, Success}
import wacc.ASTNodes._
import wacc.backend.CodeGenerator._
import wacc.backend.BasicRegisterAllocator
import wacc.frontend.ErrorOutput._
import wacc.frontend.Error._
import wacc.frontend.{SemanticAnalyser, parser}
import wacc.backend.ARMAssemblyPrinter
import wacc.backend.Instruction
import wacc.backend.Register
import wacc.extensions.Optimiser._
import wacc.extensions.StandardLibrary
import wacc.backend.PredefinedFunctions

object Main {
  val FILE_ERR_CODE = 150
  val SYNTAX_ERR_CODE = 100
  val SEMANTIC_ERR_CODE = 200
  val SUCCESS_CODE = 0

  def main(args: Array[String]): Unit = {
    println("Compiling...")

    /* Exits if arguments are not defined */
    if (args.length < 1) {
      println("Please enter an expression!")
      System.exit(FILE_ERR_CODE)
    }

    var input = ""
    var arg = args(0)
    var optimise = false

    /* Check for -o flag */
    if (arg == "-o") {
      if (args.length < 2) {
        println("Please specify an input file!")
        System.exit(FILE_ERR_CODE)
      }
      arg = args(1)
      optimise = true
    }

    /* Parses files only if argument ends with ".wacc" */
    if(arg.endsWith(".wacc")) {
      val inputFile = new File(arg)

      /* Checks if file exists */
      if (!inputFile.exists()) {
        println("File does not exist")
        System.exit(FILE_ERR_CODE)
      }

      /* Checks if input string is a file */
      if (!inputFile.isFile) {
        println("It's not a file")
        System.exit(FILE_ERR_CODE)
      }

      /* Reads file contents */
      val source = Source.fromFile(inputFile)
      val fileContents = try source.mkString finally source.close()
      input = fileContents
    }
    else {
      /* Parses string instead */
      input = arg
    }

    /* To be able to run tests */
    if (args.length > 1 && args(0) != "-o"){
      /* Invoke your parser's parse method */
      val result: Result[SyntaxError, Expr] = parser.parseTest(input)
      result match {
        case Success(x) => println(s"$arg = $x")
        case Failure(msg) => println(msg)
      }
    } else {
      /* Invoke your parser's parse method */
      val result: Result[SyntaxError, Program] = parser.parse(input)

      /* Parsing of expression */
      result match {
        case Success(ast) =>
          /* Compile standard library */
          var (stdLibInstructions, stdLibSymbolTable) = StandardLibrary.checkStdLib()

          /* Semantically Analyse AST */
          val semanticAnalyser = new SemanticAnalyser(ast, Some(stdLibSymbolTable))
          semanticAnalyser.analyse()

          var newAST = ast

          /* Perform control flow analysis on the generated AST */
          if (optimise) {
            println("AST before: " + ast)
            val newAST = controlFlowOptimise(ast)
            println("AST after: " + newAST)
          }

          /* Generate assembly instructions from AST */
          println("Generating assembly code...")
          val registerAllocator = new BasicRegisterAllocator
          val (reg, _) = registerAllocator.allocateRegister()
          var mainInstructions: ListBuffer[Instruction] = generateInstructions(newAST, registerAllocator, reg)
          var predefInstructions: ListBuffer[Instruction] = PredefinedFunctions.getPredefinedFunctions

          /* Perform control flow analysis on the generated assembly instructions */
          if (optimise) {
            println("assembly before: " + mainInstructions)
            val (optimisedMainInstructions, optimisedStdLibInstructions, optimisedPredefInstructions): 
              (ListBuffer[Instruction], ListBuffer[Instruction], ListBuffer[Instruction])
              = controlFlowOptimise(
                mainInstructions, 
                stdLibInstructions, 
                predefInstructions)
            println("assembly after: " + optimisedMainInstructions)
            
            // Replace the original instructions with the optimised ones
            mainInstructions = optimisedMainInstructions
            stdLibInstructions = optimisedStdLibInstructions
            predefInstructions = optimisedPredefInstructions
          }
          
          /* Check if the code should be optimised using the peephole */
          if (optimise) {
            println("Optimising code...")
            mainInstructions = optimiseInstructions(mainInstructions.toList)
            mainInstructions = removeComments(mainInstructions)
          }

          /* Write all pre-defined functions to file */
          PredefinedFunctions.writeToFile(predefInstructions)

          /* Write standard library to file */
          StandardLibrary.writeToFile(stdLibInstructions)

          /* Create a new file to store generated assembly for main program */
          val inputFile = new File(arg)
          val outputFileName = inputFile.getName.split('.').head + ".s"
          val file = new File(outputFileName)
          file.createNewFile()

          /* Create print writer to allow to write assembly code to file */
          val writer = new PrintWriter(file)

          /* Write assembly instructions to file using ARM assembly printer */
          ARMAssemblyPrinter.printAssembly(mainInstructions.toList, writer)
          writer.close()

          System.exit(SUCCESS_CODE)

        case Failure(msg) =>
          output(Some(msg), args(0), SYNTAX_ERR_CODE)
          System.exit(SYNTAX_ERR_CODE)
      }
    }
  }
}
