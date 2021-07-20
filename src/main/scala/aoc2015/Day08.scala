package aoc2015

import scala.io.Source
import scala.util.Using

object Day08 extends App {

  val input = Using(Source.fromFile("inputs/2015/08.txt"))(_.getLines().toList).get

  def unescapeLen(s: String) = s.foldLeft((-2, false)) {
    case ((oldLen, true), 'x') => (oldLen - 2, false)
    case ((oldLen, true), _) => (oldLen, false)
    case ((oldLen, _), '\\') => (1 + oldLen, true)
    case ((oldLen, _), _) => (1 + oldLen, false)
  }._1

  println(input.map(s => s.length - unescapeLen(s)).sum)

  def escapeLen(s: String) = s.foldLeft(2) {
    case (oldLen, '"') => 2 + oldLen
    case (oldLen, '\\') => 2 + oldLen
    case (oldLen, _) => 1 + oldLen
  }

  println(input.map(s => escapeLen(s) - s.length).sum)
}
