package aoc2015

import scala.annotation.tailrec
import scala.io.Source

object Day24 extends App {

  val input = Source.fromFile("inputs/2015/24.txt").getLines().map(_.toLong).toList

  def idealSleigh(groups: Int): Long = {

    val targetSum = input.sum / groups

    @tailrec def pick(todo: List[Long], acc: List[(Long, List[Long])]): List[List[Long]] = {
      if (todo.isEmpty) acc.filter(_._1 == targetSum).map(_._2)
      else {
        val hd = todo.head
        pick(todo.tail, if (acc.isEmpty) List((0, Nil), (hd, List(hd))) else
          acc.flatMap(pair =>
            if (hd + pair._1 <= targetSum) List((hd + pair._1, hd :: pair._2), pair)
            else List(pair))
        )
      }
    }

    val assignments = pick(input, Nil)
    val fewestNumber = assignments.map(_.size).min
    val fewest = assignments.filter(_.size == fewestNumber)
    fewest.map(_.product).min
  }

  println(idealSleigh(3))
  println(idealSleigh(4))
}
