package aoc2022

import scala.annotation.tailrec
import scala.io.Source
import scala.math.abs
import scala.util.Using

object Day09 extends App {

  sealed trait Dir

  case class R(amt: Int) extends Dir

  case class U(amt: Int) extends Dir

  case class L(amt: Int) extends Dir

  case class D(amt: Int) extends Dir

  val input = Using(Source.fromFile("inputs/2022/09.txt"))(_.getLines().map {
    case s"R $x" => R(x.toInt)
    case s"U $x" => U(x.toInt)
    case s"L $x" => L(x.toInt)
    case s"D $x" => D(x.toInt)
  }.toList).get

  def dist(head: (Int, Int), tail: (Int, Int)): Int = scala.math.max(abs(head._1 - tail._1), abs(head._2 - tail._2))

  def manhattan(head: (Int, Int), tail: (Int, Int)): Int = abs(head._1 - tail._1) + abs(head._2 - tail._2)

  def neighs(x: Int, y: Int): Set[(Int, Int)] = {
    for {
      _ <- Set(())
      xx <- x - 1 to x + 1
      yy <- y - 1 to y + 1
      if xx != x || yy != y
    } yield (xx, yy)
  }

  def follow(head: (Int, Int), tail: (Int, Int)): (Int, Int) = {
    val n = neighs(tail._1, tail._2)
    val newTail = n.filter(p => p._1 == head._1 || p._2 == head._2)
      .find(t => dist(t, head) == 1)
      .getOrElse(n.minBy(x => manhattan(x, head)))
    (newTail._1 - tail._1, newTail._2 - tail._2)
  }

  def move(pos: List[(Int, Int)], dx: Int, dy: Int): List[(Int, Int)] = {
    val newHead = (pos.head._1 + dx, pos.head._2 + dy)
    if (pos.tail.isEmpty || dist(newHead, pos.tail.head) <= 1) newHead :: pos.tail
    else {
      val (dx, dy) = follow(newHead, pos.tail.head)
      newHead :: move(pos.tail, dx, dy)
    }
  }

  @tailrec
  def move(pos: List[(Int, Int)], dir: Dir, visited: Set[(Int, Int)]): (List[(Int, Int)], Set[(Int, Int)]) = dir match {
    case R(amt) if amt > 0 => move(move(pos, 1, 0), R(amt - 1), visited + pos.last)
    case U(amt) if amt > 0 => move(move(pos, 0, -1), U(amt - 1), visited + pos.last)
    case L(amt) if amt > 0 => move(move(pos, -1, 0), L(amt - 1), visited + pos.last)
    case D(amt) if amt > 0 => move(move(pos, 0, 1), D(amt - 1), visited + pos.last)
    case _ => (pos, visited + pos.last)
  }

  println(input.foldLeft((List.fill(2)((0, 0)), Set.empty[(Int, Int)])) { case ((pos, set), m) => move(pos, m, set) }._2.size)

  println(input.foldLeft((List.fill(10)((0, 0)), Set.empty[(Int, Int)])) { case ((pos, set), m) => move(pos, m, set) }._2.size)
}
