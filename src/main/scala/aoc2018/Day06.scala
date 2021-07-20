package aoc2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day06 extends App {

  val input = Using(Source.fromFile("inputs/2018/06.txt"))(_.getLines()
    .map(_.split(", ").map(_.toInt).toList)
    .map { case a :: b :: Nil => (a, b) }
    .toList).get

  val minX = input.map(_._1).min
  val maxX = input.map(_._1).max
  val minY = input.map(_._2).min
  val maxY = input.map(_._2).max

  def neighs(seen: Set[(Int, Int)])(coord: (Int, Int)): Set[(Int, Int)] = {
    for {
      d <- Set(-1, 1)
      n <- Set(coord.copy(_1 = coord._1 + d), coord.copy(_2 = coord._2 + d))
      if n._1 >= minX && n._1 <= maxX && n._2 >= minY && n._2 <= maxY
      if !seen.contains(n)
    } yield n
  }

  def expand(coords: List[Set[(Int, Int)]]): List[Set[(Int, Int)]] = {
    val seen = coords.flatten.toSet
    val newNeighs = coords.map(_.flatMap(neighs(seen)))
    val counts = newNeighs.flatten.groupBy(identity).map {
      case (k, v) => (k, v.length)
    }
    coords.zip(newNeighs).map {
      case (old, nyu) => old ++ nyu.filter(c => counts(c) == 1)
    }
  }

  @tailrec def fixedPoint[T](update: T => T)(init: T): T = {
    val result = update(init)
    if (init == result) init
    else fixedPoint(update)(result)
  }

  val initial = input.map(coord => Set(coord))

  val p1 = fixedPoint(expand)(initial).map(_.size).max
  assert(p1 == 4186)
  println(p1)

  def sumManhattan(coord: (Int, Int)): Int = input.map(i => (i._1 - coord._1).abs + (i._2 - coord._2).abs).sum

  def expandP2(curr: (Set[(Int, Int)], Set[(Int, Int)])): (Set[(Int, Int)], Set[(Int, Int)]) = {
    val (acc, frontier) = curr
    val newFrontier = for {
      c <- frontier
      d <- Set(-1, 1)
      n <- Set(c.copy(_1 = c._1 + d), c.copy(_2 = c._2 + d))
      if !acc.contains(n) && sumManhattan(n) < 10000
    } yield n
    (acc ++ newFrontier, newFrontier)
  }

  val seed = (for {coord <- input if sumManhattan(coord) < 10000} yield coord).head

  val p2 = fixedPoint(expandP2)((Set(), Set(seed)))._1.size

  assert(p2 == 45509)
  println(p2)
}
