package aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day06 extends App {

  val input = Source.fromFile("inputs/2018/06.txt").getLines()
    .map(_.split(", ").map(_.toInt).toList)
    .map { case a :: b :: Nil => (a, b) }
    .toList

  val minX = input.map(_._1).min
  val maxX = input.map(_._1).max
  val minY = input.map(_._2).min
  val maxY = input.map(_._2).max

  def neighs(coord: (Int, Int), seen: Set[(Int, Int)]): Set[(Int, Int)] = {
    for {
      n <- Set((coord._1 - 1, coord._2), (coord._1 + 1, coord._2), (coord._1, coord._2 - 1), (coord._1, coord._2 + 1))
      if n._1 >= minX && n._1 <= maxX && n._2 >= minY && n._2 <= maxY
      if !seen.contains(n)
    } yield n
  }

  def expand(coords: List[Set[(Int, Int)]]): List[Set[(Int, Int)]] = {
    val seen = coords.flatten.toSet
    val newNeighs = coords.map(set => set.flatMap(coord => neighs(coord, seen)))
    val counts = newNeighs.flatten.groupBy(identity).map {
      case (k, v) => (k, v.length)
    }
    coords.zip(newNeighs).map {
      case (old, nyu) => old ++ nyu.filter(c => counts(c) == 1)
    }
  }

  @tailrec def fixedPoint[T](init: T, update: T => T): T = {
    val result = update(init)
    if (init == result) init
    else fixedPoint(result, update)
  }

  val initial = input.map(coord => Set(coord))

  println(fixedPoint(initial, expand).map(_.size).max)

  def sumManhattan(coord: (Int, Int)): Int = input.map(i => (i._1 - coord._1).abs + (i._2 - coord._2).abs).sum

  def expandP2(curr: (Set[(Int, Int)], Set[(Int, Int)])): (Set[(Int, Int)], Set[(Int, Int)]) = {
    val (acc, frontier) = curr
    val newFrontier = for {
      c <- frontier
      n <- Set((c._1 - 1, c._2), (c._1 + 1, c._2), (c._1, c._2 - 1), (c._1, c._2 + 1))
      if !acc.contains(n) && sumManhattan(n) < 10000
    } yield n
    (acc ++ newFrontier, newFrontier)
  }

  val seed = (for {coord <- input if sumManhattan(coord) < 10000} yield coord).head

  println(fixedPoint((Set[(Int, Int)](), Set(seed)), expandP2)._1.size)
}
