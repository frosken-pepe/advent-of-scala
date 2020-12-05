package aoc2020

import scala.io.Source

object Day05 extends App {

  def binToInt(seq: Seq[Int]): Int = seq.reverse.zipWithIndex.foldLeft(0) {
    case (acc, (b, e)) => acc + b * (1 << e)
  }

  def seatID(s: String) = {
    val a = binToInt(s.take(7).map {
      case 'F' => 0
      case 'B' => 1
    })
    val b = binToInt(s.drop(7).map {
      case 'L' => 0
      case 'R' => 1
    })
    a * 8 + b
  }

  val ids = Source.fromFile("inputs/2020/05.txt").getLines().toList.map(seatID).toSet

  println(ids.max)

  println(
    (for {
      a <- ids
      myId = a + 1
      if !ids.contains(myId) && ids.contains(myId + 1)
    } yield myId).head
  )
}
