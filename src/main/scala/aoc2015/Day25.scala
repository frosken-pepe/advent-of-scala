package aoc2015

import scala.io.Source
import scala.util.Using

object Day25 extends App {

  val re = """row (\d+), column (\d+)""".r.unanchored

  val (row, col) = Using(Source.fromFile("inputs/2015/25.txt"))(_.getLines()
    .map { case re(r, c) => (r.toInt - 1, c.toInt - 1) }.toList.head).get

  val firstCode = 20151125
  val firstCoord = (0, 0)

  def nextCode(prev: Int): Int = ((252533L * prev) % 33554393L).toInt
  def nextCoord(prev: (Int, Int)): (Int, Int) = if (prev._1 == 0) (prev._2 + 1, 0) else (prev._1 - 1, prev._2 + 1)

  println(
    LazyList.iterate(firstCode)(nextCode).zip(LazyList.iterate(firstCoord)(nextCoord))
      .dropWhile(_._2 != (row, col)).head._1)
}
