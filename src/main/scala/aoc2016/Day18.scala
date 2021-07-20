package aoc2016

import scala.io.Source
import scala.util.Using

object Day18 extends App {

  val input = Using(Source.fromFile("inputs/2016/18.txt"))(_.getLines().next().map {
    case '^' => true
    case _ => false
  }.toList).get

  def isTrap(left: Boolean, center: Boolean, right: Boolean): Boolean = (left, center, right) match {
    case (true, true, false) => true
    case (false, true, true) => true
    case (true, false, false) => true
    case (false, false, true) => true
    case _ => false
  }

  val list = LazyList.iterate(input) { prev =>
    prev.indices.map { i =>
      isTrap(
        if (i - 1 >= 0) prev(i - 1) else false,
        prev(i),
        if (i + 1 < prev.length) prev(i + 1) else false,
      )
    }.toList
  }.map(_.count(b => !b))

  println(list.take(40).sum)
  println(list.take(400000).sum)
}
