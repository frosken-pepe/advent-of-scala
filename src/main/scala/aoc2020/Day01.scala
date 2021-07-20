package aoc2020

import scala.io.Source
import scala.util.Using

object Day01 extends App {

  val input = Using(Source.fromFile("inputs/2020/01.txt"))(_.getLines().map(_.toInt).toList).get

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
