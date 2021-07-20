package aoc2020

import scala.io.Source
import scala.util.Using

object Day07 extends App {

  val myBag = "shiny gold"

  val input = {

    def parseList(str: String): List[(Int, String)] = {
      val re = """(\d+) (.*) bags?""".r
      if (str == "no other bags") Nil
      else str.split(", ").map { case re(num, col) => (num.toInt, col) }.toList
    }

    val re = """(.*) bags contain (.*)\.""".r
    Using(Source.fromFile("inputs/2020/07.txt"))(_.getLines()
      .map { case re(outerColor, innerList) => (outerColor, parseList(innerList)) }
      .toMap).get
  }

  def distinctOuterColors(inner: String): Set[String] = {
    val canContain = input.filter(_._2.exists(_._2 == inner)).keys.toSet
    canContain ++ canContain.flatMap(distinctOuterColors)
  }

  println(distinctOuterColors(myBag).size)

  def countContainingBags(outer: String): Int = {
    val contents = input(outer)
    contents.map(_._1).sum + contents.map(bag => bag._1 * countContainingBags(bag._2)).sum
  }

  println(countContainingBags(myBag))
}
