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
    val arg = args(0)

    /* Parses files only if argument ends with ".wacc" */
    if(arg.endsWith(".wacc")) {
      val inputFile = new File(args(0))

      /* Checks if file exists*/
      if (!inputFile.exists()) {
        println("File does not exist")
        System.exit(FILE_ERR_CODE)
      }

      /* Checks if input string is a file */
      if (!inputFile.isFile) {
        println("It's not a file")
        System.exit(FILE_ERR_CODE)
      }

      /*Reads file contents */
      val source = Source.fromFile(inputFile)
      val fileContents = try source.mkString finally source.close()
      input = fileContents
    }
    else {
      /* Parses string instead */
      input = arg
    }


    /* To be able to run Tests */
    if (args.length > 1) {
      // Invoke your parser's parse method
      val result: Result[SyntaxError, Expr] = parser.parseTest(input)
      result match {
        case Success(x) => println(s"$arg = $x")
        case Failure(msg) => println(msg)
      }
    } else {
      // Invoke your parser's parse method
      val result: Result[SyntaxError, Program] = parser.parse(input)

      /* Parsing of expression */
      result match {
        case Success(x) =>
          // Semantic Analysis
          val semanticAnalyser = new SemanticAnalyser(x)
          semanticAnalyser.analyse()

          // Code Generation
          val inputFile = new File(arg)
          val outputFileName = inputFile.getName.split('.').head + ".s"
          val file = new File(outputFileName)
          file.createNewFile();

          // Write a main function to the file
          val writer = new PrintWriter(file)
           writer.write(
             """
               |.intel_syntax noprefix
               |.globl main
               |.section .rodata
               |.text
               |main:
               |    push rbp
               |    push rbx
               |    mov rbp, rsp
               |    mov rax, 0
               |    pop rbx
               |    pop rbp
               |    ret
                 """.stripMargin)
//          val registerAllocator = new BasicRegisterAllocator
//          val assemblyLines = generateAssembly(x, registerAllocator.getAllRegisters)
//          assemblyLines.foreach(line => writer.write(line.format + "\n"))
          writer.close()

          System.exit(SUCCESS_CODE)

        case Failure(msg) =>
          output(Some(msg), args(0), SYNTAX_ERR_CODE)
          System.exit(SYNTAX_ERR_CODE)
      }
    }
  }
}
