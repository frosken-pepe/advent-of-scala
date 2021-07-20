package aoc2015

import scala.io.Source
import scala.util.Using

object Day05 extends App {

  val input = Using(Source.fromFile("inputs/2015/05.txt"))(_.getLines().toList).get

  def isNice1(s: String): Boolean =
    s.count("aeiou".contains(_)) >= 3 &&
      s.zip(s.drop(1)).count(it => it._1 == it._2) > 0 &&
      List("ab", "cd", "pq", "xy").forall(b => !s.contains(b))

  println(input.count(isNice1))

  val re1 = "(..).*\\1".r
  val re2 = "(.).\\1".r

  def isNice2(s: String): Boolean = re1
    .findFirstIn(s).flatMap(_ => re2.findFirstIn(s)).nonEmpty

  println(input.count(isNice2))
}
