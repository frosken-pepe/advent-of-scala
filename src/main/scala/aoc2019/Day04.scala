package aoc2019

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day04 extends App {

  val lo :: hi :: _ = Using(Source.fromFile("inputs/2019/04.txt"))(_.getLines().next().split("-").map(_.toInt).toList).get

  def groupSizes(s: String): List[Int] = {
    @tailrec def go(todo: String, acc: List[String]): List[String] = {
      if (todo.isEmpty) acc
      else if (acc.nonEmpty && todo.head == acc.head.head) go(todo.tail, s"${todo.head}${acc.head}" :: acc.tail)
      else go(todo.tail, s"${todo.head}" :: acc)
    }

    go(s, Nil).map(_.length)
  }

  @tailrec def increasing(z: String): Boolean = {
    if (z.length <= 1) true
    else z.head <= z.tail.head && increasing(z.tail)
  }

  val candidates = (lo to hi)
    .filter(Range.inclusive(100_000, 999_999) contains _)
    .map(_.toString)
    .filter(increasing)
    .map(groupSizes)

  println(candidates.count(_.exists(_ >= 2)))

  println(candidates.count(_.exists(_ == 2)))
}
