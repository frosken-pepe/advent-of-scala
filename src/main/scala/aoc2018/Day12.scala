package aoc2018

import scala.io.Source

object Day12 extends App {

  case class Rule(ll: Boolean, l: Boolean, c: Boolean, r: Boolean, rr: Boolean)

  object Input {
    private val input = Source.fromFile("inputs/2018/12.txt").getLines()

    private val re = """(.)(.)(.)(.)(.) => (.)""".r

    private def b(s: String) = s == "#"

    val initialState = input.next().drop("initial state: ".length).split("")
      .zipWithIndex
      .filter { case (s, _) => b(s) }
      .map(_._2)
      .toSet

    val rules = input.drop(1).flatMap {
      case re(ll, l, c, r, rr, "#") => Some(Rule(b(ll), b(l), b(c), b(r), b(rr)))
      case _ => None
    }.toSet
  }

  def neighs(i: Int): Set[Int] = Set(i - 2, i - 1, i, i + 1, i + 1)

  def findRule(state: Set[Int], idx: Int): Rule = {
    Rule(state.contains(idx-2), state.contains(idx-1), state.contains(idx), state.contains(idx+1), state.contains(idx+2))
  }

  def evolve(state: Set[Int]): Set[Int] = {
    state.flatMap(neighs).flatMap { idx =>
      if (Input.rules.contains(findRule(state, idx))) Some(idx)
      else None
    }
  }

  val ll = LazyList.iterate(Input.initialState)(evolve).map(_.sum)

  println(ll.drop(20).head)

  val dll = ll.drop(1).zip(ll).map(p => p._1 - p._2)

  val delta = dll.drop(1).zip(dll).filter(p => p._1 == p._2).map(_._1).head

  val drop = dll.takeWhile(_ != delta).size + 10

  println(ll.drop(drop).head + delta * (50000000000L - drop))
}
