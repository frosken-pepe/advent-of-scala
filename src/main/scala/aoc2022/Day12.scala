package aoc2022

import com.sun.media.jfxmedia.events.NewFrameEvent

import scala.io.Source
import scala.util.Using

object Day12 extends App {

  val input: Map[(Int, Int), Char] = Using(Source.fromFile("inputs/2022/12.txt"))(_.getLines().toList).get
    .zipWithIndex.flatMap { case (str, row) => str.zipWithIndex.map { case (ch, col) => (row, col) -> ch } }.toMap

  def find(ch: Set[Char]): Set[(Int, Int)] = input.filter(entry => ch(entry._2)).keySet

  def actualHeight(ch: Char): Char = ch match {
    case 'S' => 'a'
    case 'E' => 'z'
    case c => c
  }

  def height(row: Int, col: Int): Int = actualHeight(input(row, col)) - 'a'

  def neighs(row: Int, col: Int): Set[(Int, Int)] = {
    for {
      dx <- Set(-1, 0, 1)
      dy <- Set(-1, 0, 1) if dx == 0 ^ dy == 0
      nc = col + dx
      nr = row + dy
      if input.contains(nr, nc) && height(nr, nc) <= height(row, col) + 1
    } yield (nr, nc)
  }

  case class State(frontier: Set[(Int, Int)], visited: Set[(Int, Int)]) {
    def updated(newFrontier: Set[(Int, Int)]): State = {
      copy(frontier = newFrontier, visited = visited ++ newFrontier)
    }
  }

  def expand(state: State): State = state.updated(
    for {
      (row, col) <- state.frontier
      (nr, nc) <- neighs(row, col) if !state.visited((nr, nc))
    } yield (nr, nc)
  )

  val endLoc = find(Set('E')).head

  def shortestPath(start: Set[(Int, Int)]): Int = {
    LazyList.iterate(State(start, Set()))(expand).zipWithIndex.dropWhile(x => !x._1.frontier(endLoc)).head._2
  }

  println(shortestPath(find(Set('S'))))

  println(shortestPath(find(Set('S', 'a'))))
}
