package aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day16 extends App {

  case class Registry(_0: Int, _1: Int, _2: Int, _3: Int) {
    def get(i: Int): Int = i match {
      case 0 => _0
      case 1 => _1
      case 2 => _2
      case 3 => _3
    }

    def set(i: Int, v: Int): Registry = i match {
      case 0 => copy(_0 = v)
      case 1 => copy(_1 = v)
      case 2 => copy(_2 = v)
      case 3 => copy(_3 = v)
    }
  }

  object Registry {
    def apply(list: List[Int]): Registry = new Registry(
      list.head,
      list.tail.head,
      list.tail.tail.head,
      list.tail.tail.tail.head,
    )
  }

  case class Sample(before: Registry, after: Registry, instr: List[Int])

  val samples = Source.fromFile("inputs/2018/16.txt").getLines().sliding(4, 4)
    .takeWhile(_.head.startsWith("Before"))
    .map(sample)
    .toList

  val program = Source.fromFile("inputs/2018/16.txt").getLines()
    .drop(4 * samples.length + 2)
    .map(s => s.split(" ").toList.map(_.toInt))
    .toList

  def sample(seq: Seq[String]) = {
    val before = """Before: \[(\d+), (\d+), (\d+), (\d+)]""".r
    val instr = """(\d+) (\d+) (\d+) (\d+)""".r
    val after = """After: {2}\[(\d+), (\d+), (\d+), (\d+)]""".r
    Sample(
      before = seq.head match {
        case before(a, b, c, d) => Registry(List(a, b, c, d).map(_.toInt))
      },
      instr = seq.tail.head match {
        case instr(a, b, c, d) => List(a, b, c, d).map(_.toInt)
      },
      after = seq.tail.tail.head match {
        case after(a, b, c, d) => Registry(List(a, b, c, d).map(_.toInt))
      }
    )
  }

  sealed abstract class Instruction(a: Int, b: Int, c: Int) {
    def exec(reg: Registry): Registry
  }

  case class ADDR(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, reg.get(a) + reg.get(b))
  }

  case class ADDI(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, reg.get(a) + b)
  }

  case class MULR(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, reg.get(a) * reg.get(b))
  }

  case class MULI(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, reg.get(a) * b)
  }

  case class BANR(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, reg.get(a) & reg.get(b))
  }

  case class BANI(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, reg.get(a) & b)
  }

  case class BORR(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, reg.get(a) | reg.get(b))
  }

  case class BORI(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, reg.get(a) | b)
  }

  case class SETR(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, reg.get(a))
  }

  case class SETI(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, a)
  }

  case class GTIR(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, if (a > reg.get(b)) 1 else 0)
  }

  case class GTRI(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, if (reg.get(a) > b) 1 else 0)
  }

  case class GTRR(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, if (reg.get(a) > reg.get(b)) 1 else 0)
  }

  case class EQIR(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, if (a == reg.get(b)) 1 else 0)
  }

  case class EQRI(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, if (reg.get(a) == b) 1 else 0)
  }

  case class EQRR(a: Int, b: Int, c: Int) extends Instruction(a, b, c) {
    override def exec(reg: Registry): Registry = reg.set(c, if (reg.get(a) == reg.get(b)) 1 else 0)
  }

  def codes(list: List[Int]): List[Instruction] = {
    List(
      ADDR(list(1), list(2), list(3)),
      ADDI(list(1), list(2), list(3)),
      MULR(list(1), list(2), list(3)),
      MULI(list(1), list(2), list(3)),
      BANR(list(1), list(2), list(3)),
      BANI(list(1), list(2), list(3)),
      BORR(list(1), list(2), list(3)),
      BORI(list(1), list(2), list(3)),
      SETR(list(1), list(2), list(3)),
      SETI(list(1), list(2), list(3)),
      GTIR(list(1), list(2), list(3)),
      GTRI(list(1), list(2), list(3)),
      GTRR(list(1), list(2), list(3)),
      EQIR(list(1), list(2), list(3)),
      EQRI(list(1), list(2), list(3)),
      EQRR(list(1), list(2), list(3)),
    )
  }

  def codeByName(name: String, a: Int, b: Int, c: Int): Instruction = {
    codes(List(0, a, b, c)).filter(_.getClass.getSimpleName == name).head
  }

  println(samples.map(sample => codes(sample.instr).count(_.exec(sample.before) == sample.after)).count(_ >= 3))

  @tailrec def mapping(known: Map[Int, String]): Map[Int, String] = {
    val discovered = samples
      .map(sample => (sample.instr.head, codes(sample.instr)
        .filter(c => !known.values.exists(_ == c.getClass.getSimpleName))
        .filter(_.exec(sample.before) == sample.after)))
      .filter(_._2.length == 1)
      .map(p => p._1 -> p._2.head.getClass.getSimpleName)
      .head
    val result = known.updated(discovered._1, discovered._2)
    if (result.size == 16) result else mapping(result)
  }

  val map = mapping(Map())

  println(
    program.foldLeft(Registry(0, 0, 0, 0)) {
      case (reg, list) => codeByName(map(list.head), list(1), list(2), list(3)).exec(reg)
    }
  )
}
