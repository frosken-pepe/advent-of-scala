package aoc2017

import scala.io.Source

object Day04 extends App {

  val phrases = Source.fromFile("inputs/2017/04.txt").getLines()
    .map(_.split(" ").toList)
    .toList

  println(phrases.count(phrase => phrase.length == phrase.distinct.length))
  println(phrases.map(_.map(_.sorted)).count(phrase => phrase.length == phrase.distinct.length))
}
