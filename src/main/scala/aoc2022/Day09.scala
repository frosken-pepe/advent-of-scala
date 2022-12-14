package aoc2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day09 extends App {

  case class Dir(amt: Int, dx: Int, dy: Int) {
    def decr: Dir = copy(amt = amt - 1)
  }

  val input = Using(Source.fromFile("inputs/2022/09.txt"))(_.getLines().map {
    case s"R $x" => Dir(x.toInt, 1, 0)
    case s"U $x" => Dir(x.toInt, 0, -1)
    case s"L $x" => Dir(x.toInt, -1, 0)
    case s"D $x" => Dir(x.toInt, 0, 1)
  }.toList).get

  def follow(head: (Int, Int), neck: (Int, Int)): (Int, Int) = {
    val dx = head._1 - neck._1
    val dy = head._2 - neck._2
    if (dx.abs > 1 || dy.abs > 1) (dx.sign, dy.sign) else (0, 0)
  }

  type Snake = List[(Int, Int)]

  def step(snake: Snake, dx: Int, dy: Int): (Snake, (Int, Int)) = {
    @tailrec
    def go(todo: Snake, done: Snake, dx: Int, dy: Int): (Snake, (Int, Int)) = todo match {
      case Nil => (done.reverse, (done.head._1, done.head._2))
      case head :: Nil => go(Nil, (head._1 + dx, head._2 + dy) :: done, 0, 0)
      case head :: neck :: _ =>
        val moved = (head._1 + dx, head._2 + dy)
        val delta = follow(moved, neck)
        go(todo.tail, moved :: done, delta._1, delta._2)
    }

    go(snake, Nil, dx, dy)
  }

  @tailrec
  def move(snake: Snake, z: Dir, visited: Set[(Int, Int)]): (Snake, Set[(Int, Int)]) =
    if (z.amt > 0) step(snake, z.dx, z.dy) match {
      case (moved, last) => move(moved, z.decr, visited + last)
    }
    else (snake, visited)

  def snake(size: Int): Int = {
    input.foldLeft((List.fill(size)((0, 0)), Set.empty[(Int, Int)])) { case ((pos, visited), m) => move(pos, m, visited) }._2.size
  }

  println(snake(2))

  println(snake(10))
}
