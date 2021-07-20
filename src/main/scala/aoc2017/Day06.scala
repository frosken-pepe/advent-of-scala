package aoc2017

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day06 extends App {

  val input = Using(Source.fromFile("inputs/2017/06.txt"))(_.getLines()
    .next().split("\\W").map(_.toInt).zipWithIndex.map {
    case (value, index) => (index, value)
  }.toMap).get

  def maxIndex(map: Map[Int, Int]): Int = {
    (1 until map.size).foldLeft(0) {
      case (acc, idx) if map(idx) > map(acc) => idx
      case (acc, _) => acc
    }
  }

  @tailrec def distribute(map: Map[Int, Int], idx: Int, todo: Int): Map[Int, Int] =
    if (todo == 0) map
    else distribute(map.updated(idx, map(idx) + 1), (idx + 1) % map.size, todo - 1)

  def eq(map: Map[Int, Int]): List[Int] = (0 until map.size).map(map).toList

  val seq = LazyList.iterate(input) { map =>
    val maxIdx = maxIndex(map)
    distribute(map.updated(maxIdx, 0), (maxIdx + 1) % map.size, map(maxIdx))
  }.map(eq)

  val firstSeenBefore = seq.scanLeft((List[Int](), Set[List[Int]]())) {
    case ((prev, acc), list) => (list, acc + prev)
  }.drop(1).zipWithIndex.filter {
    case ((cur, seen), _) => seen.contains(cur)
  }.head

  println(firstSeenBefore._2)

  val firstSeenAt = seq.zipWithIndex.filter {
    case (item, _) => item == firstSeenBefore._1._1
  }.head

  println(firstSeenBefore._2 - firstSeenAt._2)
}
