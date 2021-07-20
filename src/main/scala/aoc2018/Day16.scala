package aoc2018

import aoc2018.Opcodes._

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day16 extends App {

  case class Sample(before: Registry, after: Registry, instr: Vector[Int])

  val lines = Using(Source.fromFile("inputs/2018/16.txt"))(_.getLines().toList).get

  val samples = lines.sliding(4, 4)
    .takeWhile(_.head.startsWith("Before"))
    .map(sample)
    .toList

  val program = lines
    .drop(4 * samples.length + 2)
    .map(s => s.split(" ").toList.map(_.toInt))

  def sample(seq: Seq[String]) = {
    val before = """Before: \[(\d+), (\d+), (\d+), (\d+)]""".r
    val instr = """(\d+) (\d+) (\d+) (\d+)""".r
    val after = """After: {2}\[(\d+), (\d+), (\d+), (\d+)]""".r
    Sample(
      before = seq.head match {
        case before(a, b, c, d) => Registry(Vector(a, b, c, d).map(_.toInt))
      },
      instr = seq.tail.head match {
        case instr(a, b, c, d) => Vector(a, b, c, d).map(_.toInt)
      },
      after = seq.tail.tail.head match {
        case after(a, b, c, d) => Registry(Vector(a, b, c, d).map(_.toInt))
      }
    )
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
    val result = known + discovered
    if (result.size == 16) result else mapping(result)
  }

  val map = mapping(Map())

  println(
    program.foldLeft(Registry(Vector.fill(4)(0))) {
      case (reg, list) => codeByName(map(list.head), list(1), list(2), list(3)).exec(reg)
    }.list.head
  )
}
