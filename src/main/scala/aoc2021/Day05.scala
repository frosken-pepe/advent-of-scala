package aoc2021

import scala.io.Source
import scala.util.Using

object Day05 extends App {

  val re = """(\d+),(\d+) -> (\d+),(\d+)""".r

  val input = Using(Source.fromFile("inputs/2021/05.txt"))(_.getLines()
    .map {
      case re(x1, y1, x2, y2) => ((x1.toInt, y1.toInt), (x2.toInt, y2.toInt))
    }
    .toList).get

  def covered(p: (Int, Int), q: (Int, Int)): List[(Int, Int)] = {
    val dx = Integer.compare(p._1, q._1)
    val dy = Integer.compare(p._2, q._2)
    q :: LazyList.iterate(p) {
      case (x, y) => (x - dx, y - dy)
    }.takeWhile(_ != q).toList
  }

  def count(lines: List[((Int, Int), (Int, Int))]): Int = {
    lines.flatMap {
      case (p, q) => covered(p, q)
    }.groupBy(identity).filter {
      case (_, v) => v.size > 1
    }.keys.size
  }

  println(count(input.filter {
    case ((x1, y1), (x2, y2)) => x1 == x2 || y1 == y2
  }))

  println(count(input))
}
