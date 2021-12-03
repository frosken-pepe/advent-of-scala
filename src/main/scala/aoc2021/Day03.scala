package aoc2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day03 extends App {

  val input = Using(Source.fromFile("inputs/2021/03.txt"))(_.getLines()
    .toList).get

  val n = input.head.length

  def mostCommon(i: Int, list: List[String], default: Char): Char = {
    val count0 = list.map(_ (i)).count(_ == '0')
    val count1 = list.length - count0
    if (count1 > count0) '1'
    else if (count1 == count0) default
    else '0'
  }

  def leastCommon(i: Int, input: List[String], default: Char): Char = {
    mostCommon(i, input, '_') match {
      case '0' => '1'
      case '1' => '0'
      case '_' => default
    }
  }

  val gamma = Integer.parseInt((for {i <- 0 until n} yield mostCommon(i, input, 'X')).mkString(""), 2)
  val epsilon = Integer.parseInt((for {i <- 0 until n} yield leastCommon(i, input, 'X')).mkString(""), 2)

  println(gamma * epsilon)

  @tailrec def oxygen(i: Int, list: List[String]): List[String] = {
    val common = mostCommon(i, list, '1')
    val newList = list.filter(_ (i) == common)
    if (newList.size == 1) newList else oxygen(i + 1, newList)
  }

  @tailrec def co2(i: Int, list: List[String]): List[String] = {
    val common = leastCommon(i, list, '0')
    val newList = list.filter(_ (i) == common)
    if (newList.size == 1) newList else co2(i + 1, newList)
  }

  val o = Integer.parseInt(oxygen(0, input).head.mkString(""), 2)
  val c = Integer.parseInt(co2(0, input).head.mkString(""), 2)

  println(o * c)
}
