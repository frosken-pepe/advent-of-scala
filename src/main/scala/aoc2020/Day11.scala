package aoc2020

import scala.io.Source
import scala.util.Using

object Day11 extends App {

  sealed trait State

  case object Floor extends State

  case object Empty extends State

  case object Occupied extends State

  type Seats = Map[(Int, Int), State]

  val seats0: Seats = Using(Source.fromFile("inputs/2020/11.txt"))(_.getLines()
    .zipWithIndex
    .flatMap {
      case (line, y) => line.zipWithIndex.map {
        case ('L', x) => (x, y) -> Empty
        case ('.', x) => (x, y) -> Floor
      }
    }
    .toMap).get

  def evolve(neighs: (Int, Int) => Set[(Int, Int)], minOccupied: Int)(seats: Seats): Seats = {
    (for {
      (x, y) <- seats.keys
      state = seats(x, y)
      occupiedNeighbors = neighs(x, y).count(n => seats(n) == Occupied)
    } yield (x, y) -> (if (state == Empty && occupiedNeighbors == 0) Occupied
    else if (state == Occupied && occupiedNeighbors >= minOccupied) Empty
    else state)).toMap
  }

  def findStableConfiguration(list: LazyList[Seats]): Int = {
    val ll = list.map(_.count(_._2 == Occupied))
    ll.zip(ll.drop(1)).dropWhile(p => p._1 != p._2).head._1
  }

  def neighsP1(x: Int, y: Int): Set[(Int, Int)] = {
    (for {
      dx <- -1 to 1
      dy <- -1 to 1
      neigh = (x + dx, y + dy) if dx != 0 || dy != 0
      if seats0 contains neigh
    } yield neigh).toSet
  }

  println(findStableConfiguration(LazyList.iterate(seats0)(evolve(neighsP1, 4))))

  val dirs = (for {
    dx <- -1 to 1
    dy <- -1 to 1
    dir = (dx, dy) if dx != 0 || dy != 0
  } yield dir).toSet

  def neighsP2(x: Int, y: Int): Set[(Int, Int)] = {
    dirs.flatMap { dir =>
      LazyList.iterate((x, y)) { case (x, y) => (x + dir._1, y + dir._2) }
        .drop(1)
        .dropWhile(p => seats0.contains(p) && seats0(p) == Floor)
        .takeWhile(seats0.contains)
        .headOption
    }
  }

  val precomputedNeighs: Map[(Int, Int), Set[(Int, Int)]] = seats0.keys.map(k => k -> neighsP2(k._1, k._2)).toMap

  println(findStableConfiguration(LazyList.iterate(seats0)(evolve((x, y) => precomputedNeighs(x, y), 5))))
}
