package aoc2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10 extends App {

  val input = Using(Source.fromFile("inputs/2021/10.txt"))(_.getLines().toList).get

  def isMatch(a: Char, b: Char): Boolean = s"$b$a" match {
    case "<>" | "[]" | "{}" | "()" => true
    case _ => false
  }

  def scoreMismatch(c: Char) = c match {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }

  @tailrec def scoreInvalid(line: String, stack: String): Option[Int] = {
    if (line.isEmpty) None
    else if (">]})".contains(line.head)) {
      if (isMatch(line.head, stack.head)) scoreInvalid(line.tail, stack.tail)
      else Some(scoreMismatch(line.head))
    }
    else scoreInvalid(line.tail, line.head + stack)
  }

  println(input.flatMap(s => scoreInvalid(s, "")).sum)

  def invert(ch: Char): Char = ch match {
    case '{' => '}'
    case '[' => ']'
    case '(' => ')'
    case '<' => '>'
  }

  @tailrec def completeLine(line: String, stack: String): String = {
    if (line.isEmpty) stack.map(invert)
    else if (">]})".contains(line.head)) completeLine(line.tail, stack.tail)
    else completeLine(line.tail, line.head + stack)
  }

  def increment(c: Char) = c match {
    case ')' => 1
    case ']' => 2
    case '}' => 3
    case '>' => 4
  }

  def scoreCompletion(s: String): BigInt = {
    s.map(increment).foldLeft(BigInt(0)) {
      case (acc, inc) => 5 * acc + inc
    }
  }

  val sorted = input.filter(s => scoreInvalid(s, "").isEmpty)
    .map(s => completeLine(s, ""))
    .map(scoreCompletion)
    .sorted

  println(sorted(sorted.size / 2))
}
