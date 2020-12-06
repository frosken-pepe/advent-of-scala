package aoc2020

import scala.io.Source

object Day6 extends App {

  val input = Source.fromFile("inputs/2020/06.txt").getLines().toList

  val groups = input.mkString("\n")
    .split("\n\n")
    .map(_.split("\n").map(_.toSet).toList)
    .toList

  println(groups.flatMap(_.flatten.toSet).size)

  val z = ('a' to 'z').toSet
  println(groups.map(g => (g :\ z) (_ intersect _)).map(_.size).sum)
}
