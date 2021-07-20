package aoc2018

import scala.io.Source
import scala.util.Using

object Day02 extends App {

  val input = Using(Source.fromFile("inputs/2018/02.txt"))(_.getLines().toList).get

  def counts(s: String) = s.groupBy(identity).values.map(_.length).toSet

  def count(i: Int) = input.map(counts).count(_.contains(i))

  println(count(2) * count(3))

  println(
    (for {
      i <- input.indices
      j <- i + 1 until input.length
      (eq, neq) = input(i).zip(input(j)).partition(p => p._1 == p._2) if neq.length == 1
    } yield eq.map(_._1).mkString).head
  )
}
