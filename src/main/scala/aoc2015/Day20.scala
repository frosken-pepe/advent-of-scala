package aoc2015

import scala.io.Source

object Day20 extends App {

  val input = Source.fromFile("inputs/2015/20.txt").getLines().next().toInt

  def listOfDivisors(num: Int): List[Int] = if (num == 1) List(1) else
    1 :: num :: LazyList.iterate(2)(_ + 1)
      .takeWhile(i => i * i <= num)
      .filter(num % _ == 0)
      .foldLeft(List[Int]()) {
        case (acc, i) if i == num / i => i :: acc
        case (acc, i) => i :: (num / i) :: acc
      }

  def sumOfDivisors(num: Int): Int = listOfDivisors(num).sum

  println(LazyList.iterate(1)(_ + 1).dropWhile(house => 10 * sumOfDivisors(house) < input).head)

  def deliversTo(house: Int)(elf: Int): Boolean = house <= 50 * elf

  println(LazyList.iterate(1)(_ + 1).dropWhile(house => 11 * listOfDivisors(house).filter(deliversTo(house)).sum < input).head)
}
