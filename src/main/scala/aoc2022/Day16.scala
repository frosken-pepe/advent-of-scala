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

  case class State(me: String, open: Set[String]) {
    def applyAction(action: Action): State = action match {
      case Open => copy(open = open + me)
      case Move(target, _) => copy(me = target)
    }
  }

  sealed trait Action

  case object Open extends Action

  case class Move(target: String, time: Int) extends Action

  val meaningfulValves = valves.values.filter(_.rate > 0).map(_.id).toSet

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
    a <- meaningfulValves + "AA"
    b <- meaningfulValves
    if a < b
  } yield (a, b, shortestPath(a, b))).flatMap {
    case (a, b, d) => List((a, b) -> d, (b, a) -> d)
  }.toMap

  def actions(state: State, time: Int, endTime: Int): Set[Action] = {
    if (state.open == meaningfulValves) Set()
    else if (!state.open(state.me) && valves(state.me).rate > 0) Set(Open)
    else meaningfulValves
      .filter(_ != state.me)
      .filter(!state.open(_))
      .map(valve => Move(valve, shortestPaths(valve, state.me)))
      .filter(_.time + time <= endTime)
      .toSet
  }

  def backtrack(time: Int, current: State, pressure: Int, visited: Set[State], endTime: Int): Int = {
    if (time == endTime) pressure
    else actions(current, time, endTime).map { a => (a, current.applyAction(a)) }
      .filter { case (_, state) => !visited(state) }
      .map {
        case (Open, s) => backtrack(time + 1, s, pressure + valves(current.me).rate * (30 - time - 1), visited + s, endTime)
        case (Move(_, deltaT), s) => backtrack(time + deltaT, s, pressure, visited + s, endTime)
      }
      .foldLeft(pressure)(math.max)
  }

  println(backtrack(0, State("AA", Set()), 0, Set(), 30))
}
