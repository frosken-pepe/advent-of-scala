package aoc2015

import scala.io.Source

object Day09 extends App {

  case class Route(from: String, to: String, dist: Int) {
    def rev: Route = new Route(to, from, dist)
  }

  object Route {
    private val route = """(\w+) to (\w+) = (\d+)""".r

    def apply(s: String): Route = s match {
      case route(from, to, dist) => new Route(from, to, dist.toInt)
    }
  }

  val input = Source.fromFile("inputs/2015/09.txt")
    .getLines()
    .map(Route.apply)
    .flatMap(route => List(route, route.rev))
    .toList

  val places = input.flatMap(route => Set(route.from, route.to)).toSet

  def reducePath(cur: String, visited: Set[String], z: Int, op: (Int, Int) => Int, acc: Int = 0): Int = {
    if (visited == places) acc
    else input.filter(route => route.from == cur && !visited.contains(route.to)).foldLeft(z) {
      case (carry, item) => op(carry, reducePath(item.to, visited + item.to, z, op, acc + item.dist))
    }
  }

  def reducePaths(z: Int, op: (Int, Int) => Int) = for {start <- places}
    yield reducePath(start, Set(start), z, op)

  println(reducePaths(Int.MaxValue, math.min).min)
  println(reducePaths(Int.MinValue, math.max).max)
}
