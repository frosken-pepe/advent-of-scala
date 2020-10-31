package aoc2016

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day24 extends App {

  case class State(openSpaces: Set[(Int, Int)], currentLocation: (Int, Int), placesToVisit: Set[(Int, Int)], initialLocation: (Int, Int))

  val input = {
    val list = Source.fromFile("inputs/2016/24.txt").getLines().toList
    val openSpaces = for {
      (row, y) <- list.zipWithIndex
      (ch, x) <- row.zipWithIndex if ch != '#'
    } yield (x, y)
    val startLocation = for {
      (row, y) <- list.zipWithIndex
      (ch, x) <- row.zipWithIndex if ch == '0'
    } yield (x, y)
    val placesToVisit = for {
      (row, y) <- list.zipWithIndex
      (ch, x) <- row.zipWithIndex if ch.isDigit && ch != '0'
    } yield (x, y)
    State(openSpaces.toSet, startLocation.head, placesToVisit.toSet, startLocation.head)
  }

  def neighs(state: State): Set[State] = {
    val neighboringLocations = for {
      delta <- Set((-1, 0), (1, 0), (0, -1), (0, 1))
      loc = (state.currentLocation._1 + delta._1, state.currentLocation._2 + delta._2)
      if (state.openSpaces.contains(loc))
    } yield loc
    neighboringLocations.map(loc =>
      state.copy(currentLocation = loc, placesToVisit = state.placesToVisit - loc)
    )
  }

  def bfs(start: State, isFinal: State => Boolean): Int = {
    val q = mutable.Queue[(Int, State)]()
    val visited = mutable.Set[State]()
    q.enqueue((0, start))
    visited.add(start)
    while (q.nonEmpty) {
      val (dist, curr) = q.dequeue()
      if (isFinal(curr)) return dist
      for {neigh <- neighs(curr)} {
        if (!visited.contains(neigh)) {
          visited.add(neigh)
          q.enqueue((dist + 1, neigh))
        }
      }
    }
    -1
  }

  @tailrec def prune(state: State): State = {
    val toRemove = for {
      coord <- (state.openSpaces - state.currentLocation) -- state.placesToVisit
      neighs = Set((coord._1 + 1, coord._2), (coord._1 - 1, coord._2), (coord._1, coord._2 + 1), (coord._1, coord._2 - 1))
      openNeighs = neighs intersect state.openSpaces
      if openNeighs.size == 1
    } yield coord
    if (toRemove.isEmpty) state
    else prune(state.copy(openSpaces = state.openSpaces -- toRemove))
  }

  println(bfs(prune(input), _.placesToVisit.isEmpty))
  println(bfs(prune(input), state => state.placesToVisit.isEmpty && state.currentLocation == state.initialLocation))
}
