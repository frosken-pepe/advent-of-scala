package aoc2018

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day09 extends App {

  object Input {
    private val input = Using(Source.fromFile("inputs/2018/09.txt"))(_.getLines().next()).get
    private val str = """(\d+) players; last marble is worth (\d+) points""".r
    val (players, worth) = input match {
      case str(p, w) => (p.toInt, w.toInt)
    }
  }

  case class Marble(value: Long, var prev: Marble, var next: Marble) {

    def insertAfter(nodeValue: Long): Marble = {
      val oldNext = next
      next = Marble(nodeValue, this, oldNext)
      oldNext.prev = next
      next
    }

    def remove: (Long, Marble) = {
      val oldNext = next
      val oldPrev = prev
      oldNext.prev = oldPrev
      oldPrev.next = oldNext
      (value, oldNext)
    }
  }

  def marbleGame(players: Int, worth: Long): Long = {
    val ptr = Marble(0, null, null)
    ptr.prev = ptr
    ptr.next = ptr
    val currentMarble = LazyList.iterate(1)(_ + 1).takeWhile(_ <= worth)
    val currentPlayer = currentMarble.map(_ - 1).map(_ % players)
    (currentMarble zip currentPlayer).foldLeft((ptr, Map[Int, Long]())) {
      case ((p, scores), (i, elf)) if i % 23L == 0L =>
        val (r, s) = p.prev.prev.prev.prev.prev.prev.prev.remove
        (s, scores.updated(elf, scores.getOrElse(elf, 0L) + i + r))
      case ((p, scores), (i, _)) => (p.next.insertAfter(i), scores)
    }._2.values.max
  }

  println(marbleGame(Input.players, Input.worth))
  println(marbleGame(Input.players, 100 * Input.worth))
}
