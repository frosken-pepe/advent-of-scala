package aoc2016

import scala.io.Source

object Day15 extends App {

  val re = """Disc #\d has (\d+) positions; at time=0, it is at position (\d+).""".r

  def disc(s: String) = s match {
    case re(n, start) => (n.toInt, start.toInt)
  }

  val input = Source.fromFile("inputs/2016/15.txt").getLines()
    .map(disc)
    .toList

  def capsule(input: List[(Int, Int)]): Int = LazyList.iterate(0)(_ + 1).map {
    t => (t, input.zipWithIndex.map { case (disc, index) => (disc._2 + t + index + 1) % disc._1 })
  }.filter {
    case (_, discs) => discs.forall(_ == 0)
  }.map(_._1).head

  println(capsule(input))
  println(capsule(input ++ List((11, 0))))
}
