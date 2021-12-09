package aoc2021

import scala.io.Source
import scala.util.Using

object Day08 extends App {

  val digits = Map(
    '0' -> "abcefg",
    '1' -> "cf",
    '2' -> "acdeg",
    '3' -> "acdfg",
    '4' -> "bcdf",
    '5' -> "abdfg",
    '6' -> "abdefg",
    '7' -> "acf",
    '8' -> "abcdefg",
    '9' -> "abcdfg",
  )

  val input = Using(Source.fromFile("inputs/2021/08.txt"))(_.getLines()
    .map(_.split('|').toList)
    .map {
      case a :: b :: Nil => (a.trim.split(" ").toList, b.trim.split(" ").toList)
    }
    .toList
  ).get

  println(input.flatMap(p => p._2).count(s => Set(2, 4, 3, 7).contains(s.length)))

  def invertChar(perm: String)(ch: Char): Char = {
    (perm.indexOf(ch) + 'a').toChar
  }

  def decode(perm: String)(s: String): String = {
    s.map(invertChar(perm)).mkString("").sorted
  }

  def valid(perm: String)(s: String): Boolean = {
    val decoded = decode(perm)(s)
    digits.values.exists(_ == decoded)
  }

  def decodeOutput(perm: String, example: List[String]): Int = {
    example.map(decode(perm))
      .map(s => digits.filter(_._2 == s).head._1)
      .mkString("")
      .toInt
  }

  def infer(example: (List[String], List[String])): Int = {
    val patterns = example._1 ++ example._2
    ('a' to 'g').permutations
      .map(_.mkString(""))
      .filter(perm => patterns.forall(valid(perm)))
      .map(decodeOutput(_, example._2))
      .next()
  }

  println(input.map(infer).sum)
}
