package wacc.extensions

import java.io.{File, PrintWriter}
import scala.io.Source
import parsley.{Result, Success}
import wacc.ASTNodes._
import wacc.backend._
import wacc.backend.CodeGenerator._
import wacc.frontend.SemanticAnalyser
import wacc.frontend.parser
import wacc.frontend.Error._
import wacc.SymbolTable

object StandardLibrary {
  def compileStdLib(): SymbolTable = {
    val stdLibFile = new File("standardLibrary.wacc")

    /* Reads file contents */
    val source = Source.fromFile(stdLibFile)
    val fileContents = try source.mkString finally source.close()

    /* Invoke your parser's parse method */
    val result: Result[SyntaxError, Program] = parser.parse(fileContents)

    /* Parsing of expression */
    result match {
      case Success(ast) =>
        /* Semantically Analyse AST */
        val semanticAnalyser = new SemanticAnalyser(ast, None)
        semanticAnalyser.analyse()

        /* Generate assembly instructions from AST */
        println("Generating assembly code...")
        val registerAllocator = new BasicRegisterAllocator
        val (reg, _) = registerAllocator.allocateRegister()
        val assemblyInstructions = generateInstructions(ast, registerAllocator, reg)
        assemblyInstructions.remove(1)
        assemblyInstructions.insertAll(1, List(Command("include \"predefinedFunctions.s\"", 0)))

        val mainInd = assemblyInstructions.indexOf(Label("main"))
        val nlInd = assemblyInstructions.indexOf(Pop(List(FP, PC)))
        println(mainInd, nlInd)
        assemblyInstructions.remove(mainInd, nlInd - mainInd + 1)

        /* Create a new file to store generated assembly */
        val file = new File("standardLibrary.s")
        file.createNewFile()

        /* Create print writer to allow to write assembly code to file */
        val writer = new PrintWriter(file)

        /* Write assembly instructions to file using ARM assembly printer */
        ARMAssemblyPrinter.printAssembly(assemblyInstructions.toList, writer)
        writer.close()
        
        semanticAnalyser.symbolTable
      case _ => throw new RuntimeException("Failed to compile standard library")
    }
  }
}
