package aoc2016

import scala.io.Source

object Day03 extends App {

  val line = """(\d+)\W+(\d+)\W+(\d+)""".r.unanchored

  val input = Source.fromFile("inputs/2016/03.txt").getLines()
    .map { case line(a, b, c) => (a.toInt :: b.toInt :: c.toInt :: Nil) }.toList

  val input2 = (for {
    x <- 0 until (input.size / 3)
    t <- 0 until 3
    a = input(3 * x)(t)
    b = input(3 * x + 1)(t)
    c = input(3 * x + 2)(t)
  } yield List(a, b, c)).toList

  def countValid(triangles: List[List[Int]]): Int =
    triangles.map(_.sorted).count { case a :: b :: c :: Nil => a + b > c }

  println(countValid(input))
  println(countValid(input2))
}
