package aoc2021

import scala.io.Source
import scala.util.Using

object Day06 extends App {

  val input = Using(Source.fromFile("inputs/2021/06.txt"))(_.getLines()
    .next()
    .split(",").map(_.toInt).toList).get

  val inputVec = (0 to 8).map {
    age => input.count(_ == age).toLong
  }.toVector

  def fishy(fish: Vector[Long]): Vector[Long] = {
    (0 to 8).toVector.map {
      case age if age == 6 => fish(0) + fish(7)
      case age if age == 8 => fish(0)
      case age => fish(age + 1)
    }
  }

  println(LazyList.iterate(inputVec)(fishy).drop(80).head.sum)
  println(LazyList.iterate(inputVec)(fishy).drop(256).head.sum)
}
