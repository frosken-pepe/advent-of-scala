package aoc2021

import scala.io.Source
import scala.util.Using

object Day07 extends App {

  val input = Using(Source.fromFile("inputs/2021/07.txt"))(_.getLines()
    .next()
    .split(",")
    .map(_.toInt)
    .toList).get

  val range = input.min to input.max

  def crabP1(end: Int): Int = {
    input.map(p => math.abs(p - end)).sum
  }

  println(range.map(crabP1).min)

  def crabP2(end: Int): Int = {
    input.map { p =>
      val n = math.abs(p - end)
      (n * (n + 1)) / 2
    }.sum
  }

  println(range.map(crabP2).min)
}
