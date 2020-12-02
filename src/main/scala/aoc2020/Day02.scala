package aoc2020

import scala.io.Source

object Day02 extends App {

  case class Policy(from: Int, to: Int, char: Char, pass: String) {
    def p1: Boolean = pass.count(_ == char) >= from && pass.count(_ == char) <= to

    def p2: Boolean = pass(from - 1) == char ^ pass(to - 1) == char
  }

  object Input {
    private val re = """(\d+)-(\d+) ([a-z]): ([a-z]+)""".r
    val input: List[Policy] = Source.fromFile("inputs/2020/02.txt").getLines()
      .map { case re(from, to, char, pass) => Policy(from.toInt, to.toInt, char.head, pass) }
      .toList
  }

  println(Input.input.count(_.p1))
  println(Input.input.count(_.p2))
}
