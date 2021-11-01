package aoc2019

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day01 extends App {

  val input = Using(Source.fromFile("inputs/2019/01.txt"))(_.getLines().map(_.toInt).toList).get

  def fuel(mass: Int): Int = mass / 3 - 2

  println(input.map(fuel).sum)

  @tailrec def totalFuel(acc: Int = 0)(mass: Int): Int = {
    val f = fuel(mass)
    if (f <= 0) acc
    else totalFuel(acc + f)(f)
  }

  println(input.map(totalFuel()).sum)
}
