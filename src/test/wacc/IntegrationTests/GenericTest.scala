package wacc.IntegrationTests

import java.nio.file.Files
import java.nio.file.Paths
import java.io.File
import parsley.{Success, Failure}

import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec
import wacc.SyntaxChecker.Parser
import wacc.SemanticChecker.SemanticChecker
import wacc.CodeGen.CodeGenerator
import wacc.Utils.BackEndUtils._

class IntegrationTest extends AnyWordSpec {
    private final val WACC_EXT_LEN = 5

    def getFiles(dir: String):List[File] = {
        val d = new File(dir)
        if (d.exists && d.isDirectory) {
            d.listFiles.filter(_.isFile).toList
        } else {
            List[File]()
        }
    }

    def getWaccFiles(dir: String):List[File] = {
        getFiles(dir).filter(_.getName.endsWith(".wacc"))
    }

    // Assume getName exists and can return the name of a file
    def getSubDirs(dir: String):List[File] = {
        val d = new File(dir)
        d.listFiles.filter(_.isDirectory).map(_.getName).toList
    }

    def testSingle(path: String, file: File)
}