package aoc2017

import scala.io.Source

object Day03 extends App {

  val input = Source.fromFile("inputs/2017/03.txt").getLines().next().toInt

  val directions = LazyList.iterate(0)(_ + 1)
  val lengths = LazyList.iterate(1)(_ + 1).flatMap(x => List(x, x))

  val spiral = directions.zip(lengths)
    .flatMap { case (dir, len) => List.fill(len)(dir) }
    .scanLeft((0, 0)) {
      case (cur, dir) => dir % 4 match {
        case 0 => (cur._1 + 1, cur._2)
        case 1 => (cur._1, cur._2 - 1)
        case 2 => (cur._1 - 1, cur._2)
        case 3 => (cur._1, cur._2 + 1)
      }
    }

  val coord = spiral.drop(input - 1).head
  println(coord._1.abs + coord._2.abs)

  def findNextValue(coord: (Int, Int), written: Map[(Int, Int), Int]): Int = {
    val neighValues = for {
      dx <- -1 to 1
      dy <- -1 to 1 if (dx, dy) != (0, 0)
      n = (coord._1 + dx, coord._2 + dy) if written.contains(n)
    } yield written(n)
    if (neighValues.isEmpty) 1 else neighValues.sum
  }

  val spiralValues = spiral.scanLeft((0, Map[(Int, Int), Int]())) {
    case ((_, written), coord) =>
      val nextValue = findNextValue(coord, written)
      (nextValue, written.updated(coord, nextValue))
  }.map(_._1).drop(1)

  println(spiralValues.dropWhile(_ < input).head)
}
