package aoc2020

import scala.io.Source

object Day15 extends App {

  val input = Source.fromFile("inputs/2020/15.txt").getLines().next().split(",").map(_.toInt).toList

  LazyList.iterate((-1, Map[Int, Int](), Map[Int, Int](), 0)) {
    case (prev, mem0, mem1, turn) =>
      val next = if (turn < input.length) input(turn)
      else if (!mem0.contains(prev)) 0
      else turn - mem0(prev) - 1
      (next, mem1, mem1.updated(next, turn), turn + 1)
  }.map(_._1).zipWithIndex.filter {
    case (_, 2020) => true
    case (_, 30_000_000) => true
    case _ => false
  }.map(_._1).take(2).foreach(println)
}
