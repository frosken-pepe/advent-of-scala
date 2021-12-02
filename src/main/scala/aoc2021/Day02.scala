package aoc2021

import scala.io.Source
import scala.util.Using

object Day02 extends App {

  val input = Using(Source.fromFile("inputs/2021/02.txt"))(_.getLines()
    .map(_.split(" ").toList)
    .map {
      case cmd :: amt :: Nil => (cmd, amt.toInt)
    }.toList).get

  val p1 = input.foldLeft((0, 0)) {
    case ((h, d), ("forward", amt)) => (h + amt, d)
    case ((h, d), ("down", amt)) => (h, d + amt)
    case ((h, d), ("up", amt)) => (h, d - amt)
  }

  println(p1._1 * p1._2)

  val p2 = input.foldLeft((0, 0, 0)) {
    case ((h, d, aim), ("forward", amt)) => (h + amt, d + aim * amt, aim)
    case ((h, d, aim), ("down", amt)) => (h, d, aim + amt)
    case ((h, d, aim), ("up", amt)) => (h, d, aim - amt)
  }

  println(p2._1 * p2._2)
}
