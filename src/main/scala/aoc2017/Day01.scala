package aoc2017

import scala.io.Source

object Day01 extends App {

  val input = Source.fromFile("inputs/2017/01.txt").getLines().next()

  val ints = input.map(ch => ch - '0')

  def sumInts(delta: Int): Int = (for {
    i <- ints.indices
    j = (i + delta) % ints.length
    if ints(i) == ints(j)
  } yield ints(i)).sum

  println(sumInts(1))
  println(sumInts(ints.length / 2))
}
