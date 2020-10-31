package aoc2016

import scala.io.Source

object Day09 extends App {

  val input = Source.fromFile("inputs/2016/09.txt").getLines().next()

  sealed class Token

  case class Marker(read: Int, repeat: Int) extends Token

  case class Text(len: Int) extends Token

  val marker = """^\((\d+)x(\d+)\)""".r.unanchored
  val text = """^(\w+)""".r.unanchored

  def parse(s: String): Option[(Token, String)] = s match {
    case marker(a, b) => Some(Marker(a.toInt, b.toInt), s.substring(3 + a.length + b.length))
    case text(z) => Some(Text(z.length), s.substring(z.length))
    case _ => None
  }

  def decodeLength(s: String, z: (Token, String) => (Long, String)): Long =
    LazyList.unfold(s) { x => parse(x) map { case (a, b) => z(a, b) } }.sum

  def part1(a: Token, b: String): (Long, String) = (a, b) match {
    case (Marker(read, repeat), rest) => (read * repeat, rest.substring(read))
    case (Text(len), rest) => (len, rest)
  }

  println(decodeLength(input, part1))

  def part2(a: Token, b: String): (Long, String) = (a, b) match {
    case (Marker(read, repeat), rest) =>
      val (a, b) = rest.splitAt(read)
      (repeat * decodeLength(a, part2), b)
    case (Text(len), rest) => (len, rest)
  }

  println(decodeLength(input, part2))
}
