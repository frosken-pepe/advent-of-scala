package aoc2017

import scala.io.Source
import scala.util.Using

object Day15 extends App {

  val re = """Generator (\w+) starts with (\d+)""".r

  val starts = Using(Source.fromFile("inputs/2017/15.txt"))(_.getLines()
    .map { case re(id, start) => (id, start.toInt) }
    .toMap).get

  val factors = Map("A" -> 16807, "B" -> 48271)

  def gen(id: String, multiplesOf: Int = 1): LazyList[Int] = {
    val fac = factors(id).toLong
    LazyList.iterate(starts(id).toLong)(x => (x * fac) % Int.MaxValue).map(_.toInt)
      .filter(_ % multiplesOf == 0)
      .drop(1)
  }

  val p1 = gen("A").zip(gen("B"))
    .take(40_000_000)
    .filter { case (a, b) => (a & 0xFFFF) == (b & 0xFFFF) }
    .foldRight(0) { case (_, acc) => acc + 1 }

  println(p1)

  val p2 = gen("A", 4).zip(gen("B", 8))
    .take(5_000_000)
    .filter { case (a, b) => (a & 0xFFFF) == (b & 0xFFFF) }
    .foldRight(0) { case (_, acc) => acc + 1 }

  println(p2)

}
