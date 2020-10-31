package aoc2015

import scala.io.Source

object Day03 extends App {

  val input = Source.fromFile("inputs/2015/03.txt").getLines().toList.head

  val dx = Map('^' -> 0, '<' -> -1, '>' -> 1, 'v' -> 0)
  val dy = Map('^' -> 1, '<' -> 0, '>' -> 0, 'v' -> -1)

  def deliver(dirs: Seq[Char]): List[(Int, Int)] = dirs.foldLeft(List((0, 0))) {
    case (route, ch) => (route.head._1 + dx(ch), route.head._2 + dy(ch)) :: route
  }

  println(deliver(input).distinct.size)

  val (santa, robby) = input.zipWithIndex.partitionMap {
    case (ch, idx) if idx % 2 == 0 => Left(ch)
    case (ch, _) => Right(ch)
  }

  println((deliver(santa) ++ deliver(robby)).distinct.size)
}
