package aoc2020

import scala.io.Source

object Day07 extends App {

  val myBag = "shiny gold"

  val input = {

    def parseList(str: String) : List[(Int, String)] = {
      val re = """(\d+) (.*) bags?""".r
      if (str == "no other bags") Nil
      else str.split(", ").map { case re(num, col) => (num.toInt, col)}.toList
    }

    val re = """(.*) bags contain (.*)\.""".r
    val lines = Source.fromFile("inputs/2020/07.txt").getLines()
      .map { case re(outerColor, innerList) => (outerColor, parseList(innerList))}

    lines.toMap
  }

  def distinctOuterColors(inner: String) : Set[String] = {
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
