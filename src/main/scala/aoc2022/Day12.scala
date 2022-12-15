package aoc2022

import scala.io.Source
import scala.util.Using

object Day12 extends App {

  val input: List[String] = Using(Source.fromFile("inputs/2022/12.txt"))(_.getLines().toList).get

  val heightmap: List[List[Int]] = input.map(_.toList.map {
    case 'S' => 'a'
    case 'E' => 'z'
    case x => x
  }.map(_ - 'a'))

  val locs = input.zipWithIndex.flatMap {
    case (str, row) => str.zipWithIndex.flatMap {
      case (ch, col) if ch == 'S' || ch == 'E' => Some(ch -> (row, col))
      case _ => None
    }
  }.toMap

  val startLoc = locs('S')
  val endLoc = locs('E')
  val width = heightmap.head.size
  val height = heightmap.size

  def neighs(row: Int, col: Int): Set[(Int, Int)] = {
    for {
      dx <- Set(-1, 0, 1)
      dy <- Set(-1, 0, 1)
      if dx == 0 || dy == 0
      nc = col + dx if nc >= 0 && nc < width
      nr = row + dy if nr >= 0 && nr < height
      if heightmap(nr)(nc) <= heightmap(row)(col) + 1
    } yield (nr, nc)
  }

  case class State(frontier: Set[(Int, Int)], visited: Set[(Int, Int)])

  def expand(state: State): State = {
    val newFrontier = for {
      (row, col) <- state.frontier
      (nr, nc) <- neighs(row, col)
      if !state.visited((nr, nc))
    } yield (nr, nc)
    State(newFrontier, state.visited ++ newFrontier)
  }

  def shortestPath(start: Set[(Int, Int)]): Int = {
    LazyList.iterate(State(start, Set()))(expand).takeWhile(!_.visited(endLoc)).size
  }

  println(shortestPath(Set(startLoc)))

  val startLocs = input.zipWithIndex.flatMap {
    case (str, row) => str.zipWithIndex.flatMap {
      case (ch, col) if ch == 'S' || ch == 'a' => Some((row, col))
      case _ => None
    }
  }.toSet

  println(shortestPath(startLocs))
}
