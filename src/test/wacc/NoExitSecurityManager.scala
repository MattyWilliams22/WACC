package wacc

import java.security.Permission

import wacc.frontend.ErrorOutput._

class NoExitSecurityManager extends SecurityManager {
  override def checkPermission(perm: java.security.Permission): Unit = {}
  override def checkPermission(perm: java.security.Permission, context: Object): Unit = {}
  override def checkExit(status: Int): Unit = {
    super.checkExit(status)
    status match {
      case FILE_ERR_CODE => throw new SecurityException("File does not exist")
      case SYNTAX_ERR_CODE => throw new SecurityException("Syntax Error")
      case SEMANTIC_ERR_CODE => throw new SecurityException("Semantic Error")
      case SUCCESS_CODE => throw new SecurityException("Program compiled successfully")
      case _ => throw new SecurityException("Unknown Error")
    }
  }
}