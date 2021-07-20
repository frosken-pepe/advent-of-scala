package aoc2015

import scala.util.Using

object Day06 extends App {

  import scala.io.Source

  abstract sealed class SantaInstruction(a: (Int, Int), b: (Int, Int)) {
    def coords: Iterator[(Int, Int)] = (for {
      x <- a._1 to b._1
      y <- a._2 to b._2
    } yield (x, y)).iterator

    def applyToLight(prev: Int, flag: Boolean): Int
  }

  case class Toggle(a: (Int, Int), b: (Int, Int)) extends SantaInstruction(a, b) {
    override def applyToLight(prev: Int, flag: Boolean): Int = if (flag) prev + 2 else 1 - prev
  }

  case class TurnOff(a: (Int, Int), b: (Int, Int)) extends SantaInstruction(a, b) {
    override def applyToLight(prev: Int, flag: Boolean): Int = if (flag) math.max(0, prev - 1) else 0
  }

  case class TurnOn(a: (Int, Int), b: (Int, Int)) extends SantaInstruction(a, b) {
    override def applyToLight(prev: Int, flag: Boolean): Int = if (flag) prev + 1 else 1
  }

  val toggle = """toggle (\d+),(\d+) through (\d+),(\d+)""".r
  val turnOff = """turn off (\d+),(\d+) through (\d+),(\d+)""".r
  val turnOn = """turn on (\d+),(\d+) through (\d+),(\d+)""".r

  def parseLine(s: String): SantaInstruction = s match {
    case toggle(ax, ay, bx, by) => Toggle((ax.toInt, ay.toInt), (bx.toInt, by.toInt))
    case turnOff(ax, ay, bx, by) => TurnOff((ax.toInt, ay.toInt), (bx.toInt, by.toInt))
    case turnOn(ax, ay, bx, by) => TurnOn((ax.toInt, ay.toInt), (bx.toInt, by.toInt))
  }

  val input = Using(Source.fromFile("inputs/2015/06.txt"))(_.getLines()
    .map(parseLine)
    .toList).get

  val z = (0 to 1000).map(_ => (0 to 1000).map(_ => 0).toArray).toArray

  def applyToMap(acc: Array[Array[Int]], p: SantaInstruction, flag: Boolean): Array[Array[Int]] = {
    val work: Array[Array[Int]] = acc.map(x => x.map(identity)) // TODO mutable...
    p.coords.map(coord => coord -> p.applyToLight(work(coord._1)(coord._2), flag)).foreach {
      case ((x, y), v) => work(x)(y) = v
    }
    work
  }

  def intensity(flag: Boolean): Int = input.foldLeft(z) {
    case (acc, p) => applyToMap(acc, p, flag)
  }.map(_.sum).sum

  println(intensity(flag = false))

  println(intensity(flag = true))
}
