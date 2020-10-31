package aoc2017

import scala.io.Source

object Day02 extends App {

  val rows = Source.fromFile("inputs/2017/02.txt")
    .getLines()
    .map(s => s.split("\\W+").map(_.toInt).toList)
    .toList

  println(rows.map(row => row.max - row.min).sum)

  println(
    (for {
      row <- rows
      a <- row
      b <- row if b > a && b % a == 0
    } yield b / a).sum
  )
}
