package aoc2021

import scala.io.Source
import scala.util.Using

object Day01 extends App {

  val input = Using(Source.fromFile("inputs/2021/01.txt"))(_.getLines()
    .map(_.toInt)
    .toList).get

  def countIncreased(list: List[Int]): Int = {
    list.zip(list.drop(1)).count { case (a, b) => b > a }
  }

  println(countIncreased(input))

  println(countIncreased(input.sliding(3, 1).map(_.sum).toList))
}
