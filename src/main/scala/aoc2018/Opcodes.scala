package aoc2018

object Opcodes {

  case class Registry(list: Vector[Int]) {
    def get(i: Int): Int = list(i)
    def set(i: Int, v: Int): Registry = Registry(list.updated(i, v))
  }

  sealed trait Instruction {
    val c: Int
    def op(reg: Registry): Int
    final def exec(reg: Registry): Registry = reg.set(c, op(reg))
  }

  case class Addr(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = reg.get(a) + reg.get(b)
  }

  case class Addi(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = reg.get(a) + b
  }

  case class Mulr(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = reg.get(a) * reg.get(b)
  }

  case class Muli(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = reg.get(a) * b
  }

  case class Banr(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = reg.get(a) & reg.get(b)
  }

  case class Bani(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = reg.get(a) & b
  }

  case class Borr(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = reg.get(a) | reg.get(b)
  }

  case class Bori(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = reg.get(a) | b
  }

  case class Setr(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = reg.get(a)
  }

  case class Seti(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = a
  }

  case class Gtir(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = if (a > reg.get(b)) 1 else 0
  }

  case class Gtri(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = if (reg.get(a) > b) 1 else 0
  }

  case class Gtrr(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = if (reg.get(a) > reg.get(b)) 1 else 0
  }

  case class Eqir(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = if (a == reg.get(b)) 1 else 0
  }

  case class Eqri(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = if (reg.get(a) == b) 1 else 0
  }

  case class Eqrr(a: Int, b: Int, c: Int) extends Instruction {
    override def op(reg: Registry): Int = if (reg.get(a) == reg.get(b)) 1 else 0
  }

  def codes(list: Vector[Int]): List[Instruction] = {
    List(
      Addr(list(1), list(2), list(3)),
      Addi(list(1), list(2), list(3)),
      Mulr(list(1), list(2), list(3)),
      Muli(list(1), list(2), list(3)),
      Banr(list(1), list(2), list(3)),
      Bani(list(1), list(2), list(3)),
      Borr(list(1), list(2), list(3)),
      Bori(list(1), list(2), list(3)),
      Setr(list(1), list(2), list(3)),
      Seti(list(1), list(2), list(3)),
      Gtir(list(1), list(2), list(3)),
      Gtri(list(1), list(2), list(3)),
      Gtrr(list(1), list(2), list(3)),
      Eqir(list(1), list(2), list(3)),
      Eqri(list(1), list(2), list(3)),
      Eqrr(list(1), list(2), list(3)),
    )
  }

  def codeByName(name: String, a: Int, b: Int, c: Int): Instruction = {
    codes(Vector(0, a, b, c)).filter(_.getClass.getSimpleName.equalsIgnoreCase(name)).head
  }

  def parseInstruction(s: String): Instruction = {
    val re = """(\w+) (\d+) (\d+) (\d+)""".r
    s match {
      case re(name, a, b, c) => codeByName(name, a.toInt, b.toInt, c.toInt)
    }
  }
}
