package aoc2015

import scala.io.Source
import scala.util.Using

object Day01 extends App {

  val input = Using(Source.fromFile("inputs/2015/01.txt"))(_.getLines().toList.head).get

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
