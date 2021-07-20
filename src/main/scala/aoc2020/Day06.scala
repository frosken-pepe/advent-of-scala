package aoc2020

import scala.io.Source
import scala.util.Using

object Day06 extends App {

  val input = Using(Source.fromFile("inputs/2020/06.txt"))(_.getLines().toList).get

  val groups = input.mkString("\n")
    .split("\n\n")
    .map(_.split("\n").map(_.toSet).toList)
    .toList

  println(groups.flatMap(_.flatten.toSet).size)

  val z = ('a' to 'z').toSet
  println(groups.map(g => (g :\ z) (_ intersect _)).map(_.size).sum)
}
