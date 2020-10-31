package aoc2015

import scala.io.Source

object Day02 extends App {

  val input = Source.fromFile("inputs/2015/02.txt").getLines().toList
    .map(_.split("x").toList.map(_.toInt).sorted)

  val part1 = input.foldLeft(0) {
    case (acc, l :: w :: h :: Nil) => acc + 3 * l * w + 2 * w * h + 2 * h * l
  }

  println(part1)

  val part2 = input.foldLeft(0) {
    case (acc, l :: w :: h :: Nil) => acc + 2 * l + 2 * w + l * w * h
  }

  println(part2)
}
