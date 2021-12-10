package aoc2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10 extends App {

  val input = Using(Source.fromFile("inputs/2021/10.txt"))(_.getLines().toList).get

  def matchChar(a: Char, b: Char): Boolean = {
    a == '>' && b == '<' ||
      a == ')' && b == '(' ||
      a == ']' && b == '[' ||
      a == '}' && b == '{'
  }

  def scoreMismatch(c: Char) = c match {
    case ')' => 3
    case ']' => 57
    case '}' => 1197
    case '>' => 25137
  }

  @tailrec def scoreInvalid(line: String, stack: String): Option[Int] = {
    if (line.isEmpty) None
    else if (">]})".contains(line.head) && !matchChar(line.head, stack.head)) Some(scoreMismatch(line.head))
    else if (">]})".contains(line.head)) scoreInvalid(line.tail, stack.tail)
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
    if (line.isEmpty) stack.map(invert).mkString("")
    else if (">]})".contains(line.head)) completeLine(line.tail, stack.tail)
    else completeLine(line.tail, line.head + stack)
  }

  def scoreCompletion(s: String): BigInt = {
    s.foldLeft(BigInt(0)) {
      case (acc, ')') => 5 * acc + 1
      case (acc, ']') => 5 * acc + 2
      case (acc, '}') => 5 * acc + 3
      case (acc, '>') => 5 * acc + 4
    }
  }

  val sorted = input.filter(s => scoreInvalid(s, "").isEmpty)
    .map(s => completeLine(s, ""))
    .map(s => scoreCompletion(s))
    .sorted

  println(sorted(sorted.size / 2))
}
