package aoc2017

import scala.io.Source
import scala.util.Using

object Day02 extends App {

  val rows = Using(Source.fromFile("inputs/2017/02.txt"))(_.getLines()
    .map(s => s.split("\\W+").map(_.toInt).toList)
    .toList).get

  println(rows.map(row => row.max - row.min).sum)

  println(
    (for {
      row <- rows
      a <- row
      b <- row if b > a && b % a == 0
    } yield b / a).sum
  )
}
