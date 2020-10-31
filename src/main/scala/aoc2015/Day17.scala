package aoc2015

import scala.io.Source

object Day17 extends App {

  val eggnog = 150

  val input = Source.fromFile("inputs/2015/17.txt").getLines().map(_.toInt).toList

  def waysToFill(remain: Int, unused: List[Int], acc: List[List[Int]] = Nil): List[List[Int]] = {
    if (remain == 0) acc
    else if (unused.isEmpty || remain < 0) Nil
    else waysToFill(remain - unused.head, unused.tail, if (acc.isEmpty) List(List(unused.head)) else acc.map(unused.head :: _)) ++
      waysToFill(remain, unused.tail, acc)
  }

  val ways = waysToFill(eggnog, input)

  println(ways.length)

  val leastAmountOfContainers = ways.map(_.length).min

  println(ways.count(_.length == leastAmountOfContainers))
}
