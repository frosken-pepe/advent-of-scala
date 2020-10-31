package aoc2015

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {

  val input = Source.fromFile("inputs/2015/10.txt").getLines().next().toList

  @tailrec def lookAndSay(todo: List[Char], acc: List[Char] = Nil): List[Char] = {
    if (todo.isEmpty) acc.reverse
    else {
      val head = todo.head
      val (prefix, rest) = todo.span(_ == head)
      lookAndSay(rest, head :: (prefix.length + '0').toChar :: acc)
    }
  }

  def calc(i: Int) = (1 to i).foldLeft(input) {
    case (prev, _) => lookAndSay(prev)
  }.size

  println(calc(40))
  println(calc(50))
}
