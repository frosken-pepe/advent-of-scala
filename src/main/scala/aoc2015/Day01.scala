package aoc2015

import scala.io.Source

object Day01 extends App {

  val input = Source.fromFile("inputs/2015/01.txt").getLines().toList.head

  val partial: (Int, Char) => Int = {
    case (acc, '(') => acc + 1
    case (acc, ')') => acc - 1
  }

  val part1 = input.foldLeft(0)(partial)

  println(part1)

  val part2 = input.scanLeft(0)(partial).zipWithIndex.collectFirst {
    case (-1, idx) => idx
  }

  println(part2)
}
