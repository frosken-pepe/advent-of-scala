package aoc2022

import scala.io.Source
import scala.util.Using

object Day16 extends App {

  case class Valve(id: String, rate: Int, neighs: Set[String])

  val re = "Valve (.+) has flow rate=(.+); tunnels? leads? to valves? (.+)".r

  def valve(s: String): Valve = s match {
    case re(id, rate, neighs) => Valve(id, rate.toInt, neighs.split(", ").toSet)
  }

  val valves: Map[String, Valve] = Using(Source.fromFile("inputs/2022/16.txt"))(_.getLines().map(valve)
    .map(valve => valve.id -> valve).toMap).get

  case class State(me: String, open: Set[String], unopened: Set[String], time: Int)

  sealed trait Action

  case object Open extends Action

  case class Move(target: String, time: Int) extends Action

  def action(state: State, action: Action): State = action match {
    case Open => state.copy(open = state.open + state.me, unopened = state.unopened - state.me, time = state.time + 1)
    case Move(target, dt) => state.copy(me = target, time = state.time + dt)
  }

  val nonZeroValves = valves.values.filter(_.rate > 0).map(_.id).toSet

  def expand(frontier: Set[String], visited: Set[String]): Set[String] = {
    frontier.flatMap(f => valves(f).neighs) -- visited
  }

  def shortestPath(from: String, to: String): Int = {
    LazyList.iterate((Set(from), Set[String]())) {
      case (frontier, visited) =>
        val newFrontier = expand(frontier, visited) -- visited
        (newFrontier, visited ++ newFrontier)
    }.zipWithIndex.dropWhile {
      case ((frontier, _), _) => !frontier.contains(to)
    }.head._2
  }

  val shortestPaths = (for {
    a <- nonZeroValves + "AA"
    b <- nonZeroValves
    if a < b
  } yield (a, b, shortestPath(a, b))).flatMap {
    case (a, b, d) => List((a, b) -> d, (b, a) -> d)
  }.toMap

  def actions(state: State, endTime: Int): Set[Action] = {
    if (state.unopened.isEmpty) Set()
    else if (!state.open(state.me) && valves(state.me).rate > 0) Set(Open)
    else state.unopened
      .map(valve => Move(valve, shortestPaths(valve, state.me)))
      .filter(_.time + state.time <= endTime)
      .toSet
  }

  def backtrack(state: State, pressure: Int, endTime: Int): Int = {
    if (state.time == endTime) pressure
    else actions(state, endTime).map {
      case Open => backtrack(action(state, Open), pressure + valves(state.me).rate * (endTime - state.time - 1), endTime)
      case Move(to, dt) => backtrack(action(state, Move(to, dt)), pressure, endTime)
    }.foldLeft(pressure)(math.max)
  }

  println(backtrack(State("AA", Set(), nonZeroValves, 0), 0, 30))

  def justTheTwoOfUs(subsets: Set[Set[String]]): Int = {
    subsets.map(State("AA", Set(), _, 0)).map(backtrack(_, 0, 26)).sum
  }

  val partitions: Set[Set[Set[String]]] = (for {
    me <- nonZeroValves.subsets()
    elephant = nonZeroValves -- me
    if (me.size - elephant.size).abs < 2 // assume approx. equal assignment of valves
    u = Set(me, elephant)
  } yield u).toSet

  println(partitions.map(justTheTwoOfUs).max)
}
