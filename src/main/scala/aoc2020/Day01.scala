package aoc2020

import scala.io.Source

object Day01 extends App {

  val input = Source.fromFile("inputs/2020/01.txt").getLines().map(_.toInt).toList

  val remainders = input.map(2020 - _).zip(input).toMap

  println((for {
    a <- input if remainders.contains(a)
    b = remainders(a)
  } yield a * b).head)

  println((for {
    i <- input.indices
    j <- i + 1 until input.length
    a = input(i) + input(j) if remainders.contains(a)
    b = remainders(a)
  } yield input(i) * input(j) * b).head)
}
