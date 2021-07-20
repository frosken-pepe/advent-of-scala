package aoc2017

import scala.io.Source
import scala.util.Using

object Day01 extends App {

  val input = Using(Source.fromFile("inputs/2017/01.txt"))(_.getLines().next()).get

  val ints = input.map(ch => ch - '0')

  def sumInts(delta: Int): Int = (for {
    i <- ints.indices
    j = (i + delta) % ints.length
    if ints(i) == ints(j)
  } yield ints(i)).sum

  println(sumInts(1))
  println(sumInts(ints.length / 2))
}
