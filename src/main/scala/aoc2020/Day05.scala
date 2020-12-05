package aoc2020

import scala.io.Source

object Day05 extends App {

  def binToInt(seq: Seq[Char], one: Char): Int = seq.foldLeft(0) {
    case (acc, `one`) => 2 * acc + 1
    case (acc, _) => 2 * acc
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
