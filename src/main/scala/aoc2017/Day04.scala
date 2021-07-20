package aoc2017

import scala.io.Source
import scala.util.Using

object Day04 extends App {

  val phrases = Using(Source.fromFile("inputs/2017/04.txt"))(_.getLines()
    .map(_.split(" ").toList)
    .toList).get

  println(phrases.count(phrase => phrase.length == phrase.distinct.length))
  println(phrases.map(_.map(_.sorted)).count(phrase => phrase.length == phrase.distinct.length))
}
