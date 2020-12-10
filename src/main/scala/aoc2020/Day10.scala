package aoc2020

import scala.collection.mutable
import scala.io.Source

object Day10 extends App {

  val joltages = Source.fromFile("inputs/2020/10.txt").getLines().map(_.toInt).toList

  val builtin = joltages.max + 3

  val all = (List(0, builtin) ++ joltages).sorted

  val diffs = all.drop(1).zip(all).map(p => p._1 - p._2).groupBy(identity).map {
    case (k,v) => (k,v.size)
  }

  println(diffs(1) * diffs(3))

  def countArrangements(todo: List[Int], prev: Int): Long = {

    val cache = mutable.Map[(Int, Int), Long]()

    def countArrangementsPrivate(todo: List[Int], prev: Int): Long = {
      if (todo.isEmpty) {
        if (builtin <= prev + 3) 1 else 0
      }
      else {
        val hd = todo.head
        val key = (hd, prev)
        if (cache.contains(key)) cache(key)
        else {
          val res = if (hd <= prev + 3)
            countArrangementsPrivate(todo.tail, hd) + countArrangementsPrivate(todo.tail, prev)
          else 0L
          cache(key) = res
          res
        }
      }
    }

    countArrangementsPrivate(todo, prev)
  }

  println(countArrangements(joltages.sorted, 0))
}
