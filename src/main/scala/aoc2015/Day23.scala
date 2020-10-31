package aoc2015

import scala.io.Source

object Day23 extends App {

  val instructions = Source.fromFile("inputs/2015/23.txt").getLines().toList
    .map(Instruction.apply)

  sealed abstract class Reg

  object Reg {
    def apply(s: String): Reg = s match {
      case "a" => A
      case "b" => B
    }
  }

  case object A extends Reg

  case object B extends Reg

  case class Registry(a: Long, b: Long) {

    def apply(reg: Reg): Long = reg match {
      case A => a
      case B => b
    }

    def map(reg: Reg, f: Long => Long): Registry = reg match {
      case A => copy(a = f(a))
      case B => copy(b = f(b))
    }
  }

  sealed abstract class Instruction {
    def exec(registry: Registry): Registry = registry

    def next(registry: Registry, pc: Int): Int = pc + 1
  }

  object Instruction {
    private val hlf = """hlf (\w)""".r
    private val tpl = """tpl (\w)""".r
    private val inc = """inc (\w)""".r
    private val jmp = """jmp ([+-]\d+)""".r
    private val jie = """jie (\w), ([+-]\d+)""".r
    private val jio = """jio (\w), ([+-]\d+)""".r

    def apply(s: String): Instruction = s match {
      case hlf(r) => HLF(Reg(r))
      case tpl(r) => TPL(Reg(r))
      case inc(r) => INC(Reg(r))
      case jmp(d) => JMP(d.toInt)
      case jie(r, d) => JIE(Reg(r), d.toInt)
      case jio(r, d) => JIO(Reg(r), d.toInt)
    }
  }

  case class HLF(r: Reg) extends Instruction {
    override def exec(registry: Registry): Registry = registry.map(r, _ / 2)
  }

  case class TPL(r: Reg) extends Instruction {
    override def exec(registry: Registry): Registry = registry.map(r, _ * 3)
  }

  case class INC(r: Reg) extends Instruction {
    override def exec(registry: Registry): Registry = registry.map(r, _ + 1)
  }

  case class JMP(offset: Int) extends Instruction {
    override def next(registry: Registry, pc: Int): Int = pc + offset
  }

  case class JIE(r: Reg, offset: Int) extends Instruction {
    override def next(registry: Registry, pc: Int): Int = if (registry(r) % 2 == 0) pc + offset else pc + 1
  }

  case class JIO(r: Reg, offset: Int) extends Instruction {
    override def next(registry: Registry, pc: Int): Int = if (registry(r) == 1) pc + offset else pc + 1
  }

  case class Program(registry: Registry, pc: Int = 0, halt: Boolean = false) {
    def advance(): Program = {
      if (halt || pc < 0 || pc >= instructions.size) copy(halt = true)
      else {
        val instr = instructions(pc)
        copy(pc = instr.next(registry, pc), registry = instr.exec(registry))
      }
    }
  }

  println(LazyList.iterate(Program(Registry(0, 0)))(_.advance()).dropWhile(p => !p.halt).head.registry.b)
  println(LazyList.iterate(Program(Registry(1, 0)))(_.advance()).dropWhile(p => !p.halt).head.registry.b)
}
