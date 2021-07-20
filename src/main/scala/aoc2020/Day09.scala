package aoc2020

import scala.io.Source
import scala.util.Using

object Day09 extends App {

  val preambleLength = 25

  val input = Using(Source.fromFile("inputs/2020/09.txt"))(_.getLines().map(_.toLong).toList).get

  def isValid(list: List[Long]): Boolean = {
    val no = list.head
    val prev = list.tail
    (for {
      i <- prev.indices
      j <- i + 1 until prev.length
      if prev(i) + prev(j) == no
    } yield ()).nonEmpty
  }

  val p1 = input.sliding(preambleLength + 1).map(_.reverse).dropWhile(isValid).toList.head.head

  println(p1)

  val seq = LazyList.iterate(2)(_ + 1).flatMap(input.sliding).filter(_.sum == p1).head

  println(seq.min + seq.max)
}
