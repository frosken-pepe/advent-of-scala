package aoc2022

import scala.io.Source
import scala.util.Using

object Day02 extends App {

  private val strategy: List[(Char, Char)] = Using(Source.fromFile("inputs/2022/02.txt"))(_.getLines().map {
    case s"$a $x" => a.head -> x.head
  }.toList).get

  def outcome(opponent: Char, you: Char): Int = (opponent, you) match {
    case (x, y) if x == y => 3
    case ('A', 'B') | ('B', 'C') | ('C', 'A') => 6
    case _ => 0
  }

  def score(opponent: Char, you: Char)(chooser: (Char, Char) => Char): Int = {
    val yourMove = chooser(opponent, you)
    outcome(opponent, yourMove) + (yourMove match {
      case 'A' => 1
      case 'B' => 2
      case 'C' => 3
    })
  }

  private def chooseMoveP1(opponent: Char, you: Char): Char = you match {
    case 'X' => 'A'
    case 'Y' => 'B'
    case 'Z' => 'C'
  }

  println(strategy.map { case (o, y) => score(o, y)(chooseMoveP1) }.sum)

  private def chooseMoveP2(opponent: Char, you: Char): Char = you match {
    case 'X' => ('A' to 'C').find(z => outcome(opponent, z) == 0).get
    case 'Y' => ('A' to 'C').find(z => outcome(opponent, z) == 3).get
    case 'Z' => ('A' to 'C').find(z => outcome(opponent, z) == 6).get
  }

  println(strategy.map { case (o, y) => score(o, y)(chooseMoveP2) }.sum)
}
