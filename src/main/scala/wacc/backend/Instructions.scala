package wacc.backend

// Intel syntax x86-64 instructions

object Instruction {

  sealed trait Operand {
    def format: String
  }

  case class ImmVal(value: Int) extends Operand {
    override def format: String = "#" + value.toString
  }

  sealed trait AssemblyLine {
    def format: String
  }

  case class Label(name: String) extends AssemblyLine {
    override def format: String = name + ":"
  }

  case class Push(reg: Register) extends AssemblyLine {
    override def format: String = s"push ${reg.format}"
  }

  case class Pop(reg: Register) extends AssemblyLine {
    override def format: String = s"pop ${reg.format}"
  }

  case class Mov(dest: Register, src: Operand) extends AssemblyLine {
    override def format: String = s"mov ${dest.format}, ${src.format}"
  }

  case class Lea(dest: Register, src: Operand) extends AssemblyLine {
    override def format: String = s"lea ${dest.format}, ${src.format}"
  }

  case class Add(dest: Register, src: Operand) extends AssemblyLine {
    override def format: String = s"add ${dest.format}, ${src.format}"
  }

  case class Sub(dest: Register, src: Operand) extends AssemblyLine {
    override def format: String = s"sub ${dest.format}, ${src.format}"
  }

  case class Mul(dest: Register, src: Operand) extends AssemblyLine {
    override def format: String = s"imul ${dest.format}, ${src.format}"
  }

  case class Div(dest: Register, src: Operand) extends AssemblyLine {
    override def format: String = s"idiv ${src.format}"
  }

  case class Cmp(reg1: Register, reg2: Register) extends AssemblyLine {
    override def format: String = s"cmp ${reg1.format}, ${reg2.format}"
  }

  case class Jmp(label: Label) extends AssemblyLine {
    override def format: String = s"jmp ${label.format}"
  }

  case class Je(label: Label) extends AssemblyLine {
    override def format: String = s"je ${label.format}"
  }

  case class Jne(label: Label) extends AssemblyLine {
    override def format: String = s"jne ${label.format}"
  }

  case class Jg(label: Label) extends AssemblyLine {
    override def format: String = s"jg ${label.format}"
  }

  case class Jge(label: Label) extends AssemblyLine {
    override def format: String = s"jge ${label.format}"
  }

  case class Jl(label: Label) extends AssemblyLine {
    override def format: String = s"jl ${label.format}"
  }

  case class Jle(label: Label) extends AssemblyLine {
    override def format: String = s"jle ${label.format}"
  }

  case class Call(label: Label) extends AssemblyLine {
    override def format: String = s"call ${label.format}"
  }

  case class Ret() extends AssemblyLine {
    override def format: String = "ret"
  }

  case class Asciz(str: String) extends AssemblyLine {
    override def format: String = s".asciz $str"
  }
}