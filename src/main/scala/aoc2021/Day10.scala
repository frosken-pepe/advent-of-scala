package aoc2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10 extends App {

  val input = Using(Source.fromFile("inputs/2021/10.txt"))(_.getLines()
    .map(_.toList)
    .toList).get

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

  def scoreInvalid(line: List[Char]): Option[Int] = {
    @tailrec def go(todo: List[Char], acc: List[Char]): Option[Int] = {
      if (todo.isEmpty) None
      else if (">]})".contains(todo.head)) {
        if (isMatch(todo.head, acc.head)) go(todo.tail, acc.tail)
        else Some(scoreMismatch(todo.head))
      }
      else go(todo.tail, todo.head :: acc)
    }

    go(line, Nil)
  }

  println(input.flatMap(scoreInvalid).sum)

  def invert(ch: Char): Char = ch match {
    case '{' => '}'
    case '[' => ']'
    case '(' => ')'
    case '<' => '>'
  }

  def completeLine(line: List[Char]): List[Char] = {
    @tailrec def go(todo: List[Char], acc: List[Char]): List[Char] = {
      if (todo.isEmpty) acc.map(invert)
      else if (">]})".contains(todo.head)) go(todo.tail, acc.tail)
      else go(todo.tail, todo.head :: acc)
    }

    go(line, Nil)
  }

  def increment(c: Char) = c match {
    case ')' => 1
    case ']' => 2
    case '}' => 3
    case '>' => 4
  }

  def scoreCompletion(s: List[Char]): BigInt = {
    s.map(increment).foldLeft(BigInt(0)) {
      case (acc, inc) => 5 * acc + inc
    }
  }

  val sorted = input.filter(scoreInvalid(_).isEmpty)
    .map(completeLine)
    .map(scoreCompletion)
    .sorted

  println(sorted(sorted.size / 2))
}
