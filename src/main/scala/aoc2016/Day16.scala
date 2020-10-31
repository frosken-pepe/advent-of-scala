package aoc2016

import scala.annotation.tailrec
import scala.io.Source

object Day16 extends App {

  val input = Source.fromFile("inputs/2016/16.txt").getLines().next()

  def next(a: String): String = {
    val b = a.reverse.map {
      case '0' => '1'
      case '1' => '0'
    }.mkString
    a + "0" + b
  }

  @tailrec def checksum(s: String): String = {
    val temp = s.sliding(2, 2).map {
      case "00" => "1"
      case "11" => "1"
      case _ => "0"
    }.mkString
    if (temp.length % 2 == 0) checksum(temp) else temp
  }

  def fill(initial: String, size: Int): String =
    checksum(LazyList.iterate(initial)(next).dropWhile(s => s.length < size).head.substring(0, size))

  println(fill(input, 272))
  println(fill(input, 35651584))
}
