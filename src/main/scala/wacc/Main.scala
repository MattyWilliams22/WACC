package wacc

import java.io.{File, PrintWriter}
import scala.io.Source
import parsley.{Failure, Result, Success}
import wacc.ASTNodes._
import wacc.backend.CodeGenerator._
import wacc.backend.BasicRegisterAllocator
import wacc.frontend.ErrorOutput._
import wacc.frontend.Error._
import wacc.frontend.{SemanticAnalyser, parser}
import wacc.backend.ARMAssemblyPrinter
import wacc.extensions.Optimiser._

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
          /* Semantically Analyse AST */
          val semanticAnalyser = new SemanticAnalyser(ast)
          semanticAnalyser.analyse()

          /* Generate assembly instructions from AST */
          println("Generating assembly code...")
          val registerAllocator = new BasicRegisterAllocator
          val (reg, _) = registerAllocator.allocateRegister()
          var assemblyInstructions = generateInstructions(ast, registerAllocator, reg)

          /* Check if the code should be optimised */
          if (optimise) {
            println("Optimising code...")
            assemblyInstructions = optimiseInstructions(assemblyInstructions.toList)
          }

          /* Create a new file to store generated assembly */
          val inputFile = new File(arg)
          val outputFileName = inputFile.getName.split('.').head + ".s"
          val file = new File(outputFileName)
          file.createNewFile()

          /* Create print writer to allow to write assembly code to file */
          val writer = new PrintWriter(file)

          /* Write assembly instructions to file using ARM assembly printer */
          ARMAssemblyPrinter.printAssembly(assemblyInstructions.toList, writer)
          writer.close()

          System.exit(SUCCESS_CODE)

        case Failure(msg) =>
          output(Some(msg), args(0), SYNTAX_ERR_CODE)
          System.exit(SYNTAX_ERR_CODE)
      }
    }
  }
}
