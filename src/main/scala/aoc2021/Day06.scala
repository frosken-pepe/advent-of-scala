package aoc2021

import scala.io.Source
import scala.util.Using

object Day06 extends App {

  val input = Using(Source.fromFile("inputs/2021/06.txt"))(_.getLines()
    .next()
    .split(",").map(_.toInt).toList).get

  val inputMap = (0 to 8).map {
    age => age -> input.count(_ == age).toLong
  }.toMap

  def fishy(fish: Map[Int, Long]): Map[Int, Long] = {
    (0 to 8).map {
      case age if age == 6 => age -> (fish(0) + fish(7))
      case age if age == 8 => age -> fish(0)
      case age => age -> fish(age + 1)
    }.toMap
  }

  println(LazyList.iterate(inputMap)(fishy).drop(80).head.values.sum)
  println(LazyList.iterate(inputMap)(fishy).drop(256).head.values.sum)
}
