package aoc2020

import scala.io.Source

object Day05 extends App {

  def binToInt(seq: Seq[Char], one: Char): Int = seq.foldLeft(0) {
    case (acc, b) => 2 * acc + (if (b == one) 1 else 0)
  }

  def seatID(s: String) = {
    8 * binToInt(s.take(7), 'B') + binToInt(s.drop(7), 'R')
  }

  val ids = Source.fromFile("inputs/2020/05.txt").getLines().map(seatID).toSet

  println(ids.max)

  println(
    (for {
      a <- ids
      myId = a + 1
      if !ids.contains(myId) && ids.contains(myId + 1)
    } yield myId).head
  )
}
