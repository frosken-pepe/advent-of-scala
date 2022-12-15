package aoc2022

import aoc2018.Day17.fixedPoint

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day14 extends App {

  type Vertex = (Int, Int)
  type Path = List[Vertex]

  def vertex(s: String): Vertex = s.split(",").toList match {
    case a :: b :: _ => (a.toInt, b.toInt)
  }

  def path(s: String): Path = {
    s.split(" -> ").map(vertex).toList
  }

  val input = Using(Source.fromFile("inputs/2022/14.txt"))(_.getLines().map(path).toSet).get

  def segment(from: Vertex, to: Vertex): Set[Vertex] = {
    val dx = (to._1 - from._1).sign
    val dy = (to._2 - from._2).sign
    Set(to) ++ LazyList.iterate(from)(vert => (vert._1 + dx, vert._2 + dy)).takeWhile(_ != to).toSet
  }

  val rocks: Set[Vertex] = input.flatMap(path => path.zip(path.drop(1)).flatMap { case (a, b) => segment(a, b) }.toSet)

  val source: Vertex = (500, 0)

  val floor = rocks.map(_._2).max + 2

  def rock(vertex: (Int, Int)): Boolean = vertex match {
    case (x, y) => y == floor || rocks(x, y)
  }

  case class State(moving: Option[Vertex], settled: Set[Vertex])

  private def spots(vertex: Vertex): List[Vertex] = vertex match {
    case (x, y) => List((x, y + 1), (x - 1, y + 1), (x + 1, y + 1))
  }

  private def move(moving: Vertex, settled: Set[Vertex]): Option[Vertex] = {
    spots(moving).find(loc => !settled(loc) && !rock(loc))
  }

  @tailrec
  def settle(isFinal: Vertex => Boolean)(state: State): State = state match {
    case State(None, _) => state
    case State(Some(moving), settled) => move(moving, settled) match {
      case Some(vertex) if !isFinal(vertex) => settle(isFinal)(State(moving = Some(vertex), settled))
      case None => settle(isFinal)(State(None, settled + moving))
      case _ => settle(isFinal)(State(None, settled))
    }
  }

  def addMoreSand(isFinal: Vertex => Boolean)(state: State): State = {
    settle(isFinal)(state.copy(moving = Some(source)))
  }

  println(fixedPoint(addMoreSand(_._2 == floor - 1))(State(None, Set())).settled.size)

  println(fixedPoint(addMoreSand(_ => false))(State(None, Set())).settled.size)
}
