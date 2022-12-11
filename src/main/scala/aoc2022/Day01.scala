package aoc2022

import scala.io.Source
import scala.util.Using

object Day01 extends App {

  val input: List[List[Int]] = Using(Source.fromFile("inputs/2022/01.txt"))(_.getLines().toList
    .mkString("\n")
    .split("\n\n")
    .map(_.split("\n").map(_.toInt).toList)
    .toList).get

  println(input.map(_.sum).max)

  println(input.map(_.sum).sorted.reverse.take(3).sum)
}
