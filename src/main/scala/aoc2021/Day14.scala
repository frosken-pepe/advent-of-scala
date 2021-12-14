package aoc2021

import scala.io.Source
import scala.util.Using

object Day14 extends App {

  val input = Using(Source.fromFile("inputs/2021/14.txt"))(_.getLines()
    .toList).get

  val template: String = input.head
  val rules: Map[String, String] = input.drop(2).map {
    case s"$a -> $b" => a -> b
  }.toMap
  val last: Char = template.takeRight(1).head

  def step(pairs: Map[String, Long]): Map[String, Long] = {
    pairs.toList.flatMap {
      case (p, cnt) => List(("" + p(0) + rules(p), cnt), ("" + rules(p) + p(1), cnt))
    }.groupBy(_._1).map {
      case (str, value) => str -> value.map(_._2).sum
    }
  }

  def mapFrom(s: String): Map[String, Long] = {
    s.sliding(2).toList.groupBy(identity).map {
      case (str, value) => str -> value.length
    }
  }

  def maxMinDiff(steps: Int): Long = {
    val pairs = LazyList.iterate(mapFrom(template))(step).drop(steps).head
    val chars = pairs.keys.flatten.toSet
    val counts = chars.map(ch => pairs.keys.filter(key => key.head == ch).map(pairs).sum
      + (if (ch == last) 1 else 0))
    counts.max - counts.min
  }

  println(maxMinDiff(10))
  println(maxMinDiff(40))
}
