package aoc2020

import scala.io.Source

object Day03 extends App {

  val input = Source.fromFile("inputs/2020/03.txt").getLines().toList
    .zipWithIndex.flatMap {
    case (str, y) => str.zipWithIndex.flatMap {
      case (ch, x) if ch == '#' => Some((x, y))
      case _ => None
    }
  }.toSet

  val maxX = input.map(_._1).max
  val maxY = input.map(_._2).max

  def trees(dx: Int, dy: Int) = LazyList.iterate((0, 0)) {
    case (x, y) => (x + dx, y + dy)
  }.takeWhile(_._2 <= maxY).count {
    case (x, y) => input.contains((x % (maxX + 1), y % (maxY + 1)))
  }

  println(trees(3, 1))

  println(trees(1, 1) * trees(3, 1) * trees(5, 1) * trees(7, 1) * trees(1, 2))
}
