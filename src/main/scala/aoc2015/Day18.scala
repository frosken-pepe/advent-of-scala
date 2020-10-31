package aoc2015

import scala.io.Source

object Day18 extends App {

  val input = Source.fromFile("inputs/2015/18.txt").getLines().toList
    .zipWithIndex
    .flatMap { case (line, y) => line.zipWithIndex.map { case (ch, x) => (x, y, ch) } }
    .filter(_._3 == '#')
    .map { case (x, y, _) => (x, y) }
    .toSet

  val deltas = Set(-1, 0, 1)

  def neighs(coord: (Int, Int)): Set[(Int, Int)] = (for {
    dx <- deltas
    dy <- deltas
    neigh = (coord._1 + dx, coord._2 + dy) if dx != 0 || dy != 0
  } yield neigh).filter { case (x, y) => x >= 0 && y >= 0 && x < 100 && y < 100 }

  def next(coord: (Int, Int), on: Set[(Int, Int)]): Boolean = {
    val onNeighs = neighs(coord).intersect(on).size
    if (on.contains(coord)) 2 == onNeighs || 3 == onNeighs else 3 == onNeighs
  }

  def iterate(alwaysOn: Set[(Int, Int)])(on: Set[(Int, Int)]): Set[(Int, Int)] = alwaysOn ++ (for {
    light <- on
    neigh <- neighs(light) if next(neigh, on)
  } yield neigh)

  val corners = Set((0, 0), (0, 99), (99, 0), (99, 99))

  println(LazyList.iterate(input)(iterate(Set())).drop(100).head.size)
  println(LazyList.iterate(input)(iterate(corners)).drop(100).head.size)
}
