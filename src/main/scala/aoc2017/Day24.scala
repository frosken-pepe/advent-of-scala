package aoc2017

import scala.io.Source

object Day24 extends App {

  val input = Source.fromFile("inputs/2017/24.txt").getLines()
    .map(line => line.split("/").toList.map(_.toInt))
    .map { case a :: b :: Nil => (a, b) }
    .toList
    .zipWithIndex.toSet

  case class State(bridge: List[(Int, Int)], used: Set[Int]) {
    def strength: Int = bridge.map(p => p._1 + p._2).sum
    def length: Int = bridge.length
  }

  object State {
    implicit val order: Ordering[State] = (x: State, y: State) => {
      val cmpLength = x.length.compareTo(y.length)
      if (cmpLength == 0) x.strength.compareTo(y.strength) else cmpLength
    }
  }

  def neighs(state: State): Set[State] = {
    val hd = if (state.bridge.isEmpty) 0 else state.bridge.head._1
    input.filter(p => p._1._1 == hd || p._1._2 == hd).filter(p => !state.used.contains(p._2))
      .map {
        case (p@(_, `hd`), idx) => State(p :: state.bridge, state.used + idx)
        case (p@(`hd`, _), idx) => State(p.swap :: state.bridge, state.used + idx)
      }
  }

  val state = State(Nil, Set())

  def maxStrength(state: State): Int = {
    val ns = neighs(state)
    if (ns.isEmpty) state.strength
    else ns.map(maxStrength).max
  }

  def longest(state: State): State = {
    val ns = neighs(state)
    if (ns.isEmpty) state
    else ns.map(longest).max
  }

  println(maxStrength(state))
  println(longest(state).strength)
}
