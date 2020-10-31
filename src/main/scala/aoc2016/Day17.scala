package aoc2016

import aoc2015.Day04.md5

import scala.io.Source

object Day17 extends App {

  val input = Source.fromFile("inputs/2016/17.txt").getLines().next()

  case class State(x: Int, y: Int, path: String)

  def neighs(state: State): List[State] =
    md5(input + state.path).substring(0, 4).toLowerCase.toList
      .map(ch => ch >= 'b')
      .zip(List(
        ('U', state.x, state.y - 1),
        ('D', state.x, state.y + 1),
        ('L', state.x - 1, state.y),
        ('R', state.x + 1, state.y)))
      .filter(_._1)
      .map(_._2)
      .filter { case (_, x, y) => x >= 0 && y >= 0 && x < 4 && y < 4 }
      .map { case (ch, x, y) => State(x, y, state.path + ch) }

  def isFinal(state: State): Boolean = state.x == 3 && state.y == 3

  def findPaths(state: State): List[String] =
    if (isFinal(state)) List(state.path)
    else neighs(state).flatMap(findPaths)

  val paths = findPaths(State(0, 0, ""))
  println(paths.minBy(_.length))
  println(paths.map(_.length).max)
}
