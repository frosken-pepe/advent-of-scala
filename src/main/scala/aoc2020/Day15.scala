package aoc2020

import scala.io.Source

object Day15 extends App {

  val input = Source.fromFile("inputs/2020/15.txt").getLines().next().split(",").map(_.toInt).toList

  LazyList.iterate((-1, 0, Map[Int, Int]())) {
    case (prev, turn, mem) =>
      val next = if (turn < input.length) input(turn)
      else if (!mem.contains(prev)) 0
      else turn - mem(prev)
      (next, turn + 1, mem.updated(prev, turn))
  }.map(_._1).zipWithIndex.filter {
    case (_, 2020) => true
    case (_, 30_000_000) => true
    case _ => false
  }.map(_._1).take(2).foreach(println)
}
