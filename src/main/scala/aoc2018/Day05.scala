package aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day05 extends App {

  val input = Source.fromFile("inputs/2018/05.txt").getLines().next().toList

  def oppositePolarity(a: Char, b: Char): Boolean = a.isUpper != b.isUpper && a.toLower == b.toLower

  @tailrec def reactLength(todo: List[Char], acc: List[Char])(ignore: Char): Int = {
    if (todo.isEmpty) acc.length
    else {
      val hd = todo.head
      if (hd == ignore || oppositePolarity(hd, ignore)) reactLength(todo.tail, acc)(ignore)
      else if (acc.isEmpty) reactLength(todo.tail, todo.head :: acc)(ignore)
      else if (oppositePolarity(acc.head, hd)) reactLength(todo.tail, acc.tail)(ignore)
      else reactLength(todo.tail, hd :: acc)(ignore)
    }
  }

  println(reactLength(input, Nil)('*'))

  println(('a' to 'z').map(reactLength(input, Nil)).min)
}
