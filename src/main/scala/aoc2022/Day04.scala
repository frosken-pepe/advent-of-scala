package aoc2022

import scala.io.Source
import scala.util.Using

object Day04 extends App {

  private val input: List[((Int, Int), (Int, Int))] = Using(Source.fromFile("inputs/2022/04.txt"))(_.getLines()
    .map {
      case s"$a-$b,$c-$d" => ((a.toInt, b.toInt), (c.toInt, d.toInt))
    }.toList).get

  def contains(a: (Int, Int), b: (Int, Int)) = {
    a._1 >= b._1 && a._2 <= b._2
  }

  println(input.count {
    case (a, b) => contains(a, b) || contains(b, a)
  })

  def overlaps(a: (Int, Int), b: (Int, Int)) = {
    a._2 >= b._1 && b._2 >= a._1
  }

  println(input.count {
    case (a, b) => overlaps(a, b)
  })
}
