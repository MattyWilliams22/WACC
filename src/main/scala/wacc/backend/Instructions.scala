package wacc.backend

// Intel syntax x86-64 instructions

object Instructions {

  trait Operand {
    def format: String
  }

  case class ImmVal(value: Int) extends Operand {
    override def format: String = "#" + value.toString
  }

  sealed trait AssemblyLine {
    def format: String
  }

  case class Comment(comment: String) extends AssemblyLine {
    override def format: String = s"# $comment"
  }

  case class Label(name: String) extends AssemblyLine {
    override def format: String = name + ":"
  }

  case class Push(reg: Register) extends AssemblyLine {
    override def format: String = s"        push ${reg.format}"
  }

  case class Pop(reg: Register) extends AssemblyLine {
    override def format: String = s"        pop ${reg.format}"
  }

  case class Mov(dest: Register, src: Operand) extends AssemblyLine {
    override def format: String = s"        mov ${dest.format}, ${src.format}"
  }

  case class Lea(dest: Register, src: Operand) extends AssemblyLine {
    override def format: String = s"        lea ${dest.format}, ${src.format}"
  }

  case class AddInstr(dest: Register, src: Operand) extends AssemblyLine {
    override def format: String = s"        add ${dest.format}, ${src.format}"
  }

  case class SubInstr(dest: Register, src: Operand) extends AssemblyLine {
    override def format: String = s"        sub ${dest.format}, ${src.format}"
  }

  case class MulInstr(dest: Register, src: Operand) extends AssemblyLine {
    override def format: String = s"        imul ${dest.format}, ${src.format}"
  }

  case class DivInstr(dest: Register, src: Operand) extends AssemblyLine {
    override def format: String = s"        idiv ${src.format}"
  }

  case class Cmp(reg1: Register, reg2: Register) extends AssemblyLine {
    override def format: String = s"        cmp ${reg1.format}, ${reg2.format}"
  }

  case class Jmp(label: String) extends AssemblyLine {
    override def format: String = s"        jmp $label"
  }

  case class Je(label: String) extends AssemblyLine {
    override def format: String = s"        je $label"
  }

  case class Jne(label: String) extends AssemblyLine {
    override def format: String = s"        jne $label"
  }

  case class Jg(label: String) extends AssemblyLine {
    override def format: String = s"        jg $label"
  }

  case class Jge(label: String) extends AssemblyLine {
    override def format: String = s"        jge $label"
  }

  case class Jl(label: String) extends AssemblyLine {
    override def format: String = s"        jl $label"
  }

  case class Jle(label: String) extends AssemblyLine {
    override def format: String = s"        jle $label"
  }

  case class CallInstr(label: String) extends AssemblyLine {
    override def format: String = s"        call $label"
  }

  case class Ret() extends AssemblyLine {
    override def format: String = "        ret"
  }

  case class Asciz(str: String) extends AssemblyLine {
    override def format: String = s"        .asciz $str"
  }
}