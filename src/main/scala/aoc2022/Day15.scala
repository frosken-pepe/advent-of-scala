package aoc2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day15 extends App {

  type Vertex = (Int, Int)
  type Reading = (Vertex, Vertex)

  def reading(s: String): Reading = s match {
    case s"Sensor at x=$xs, y=$ys: closest beacon is at x=$xb, y=$yb" => ((xs.toInt, ys.toInt), (xb.toInt, yb.toInt))
  }

  val input: Map[Vertex, Vertex] = Using(Source.fromFile("inputs/2022/15.txt"))(_.getLines().map(reading).toMap).get

  def manhattan(reading: Reading): Int = {
    (reading._1._1 - reading._2._1).abs + (reading._1._2 - reading._2._2).abs
  }

  def excludedRange(y: Int, reading: Reading): Range.Inclusive = {
    val delta = manhattan(reading) - (reading._1._2 - y).abs
    reading._1._1 - delta to reading._1._1 + delta
  }

  def limit(lo: Int, hi: Int)(range: Range.Inclusive): Option[Range.Inclusive] = {
    if (range.isEmpty) None
    else Some(math.max(lo, range.min) to math.min(hi, range.max)).filter(_.nonEmpty)
  }

  @tailrec
  def mergeAdjacent(todo: List[Range.Inclusive], acc: List[Range.Inclusive]): List[Range.Inclusive] = todo match {
    case Nil => acc.reverse
    case hd :: tl if acc.isEmpty && hd.nonEmpty => mergeAdjacent(tl, hd :: acc)
    case hd :: tl if acc.head.max + 1 >= hd.min => mergeAdjacent(tl, (acc.head.min to math.max(acc.head.max, hd.max)) :: acc.tail)
    case hd :: tl => mergeAdjacent(tl, hd :: acc)
  }

  def foldAll(lo: Int, hi: Int)(y: Int): List[Range.Inclusive] = {
    input.foldLeft(List.empty[Range.Inclusive]) {
      case (ranges, reading) => limit(lo, hi)(excludedRange(y, reading)).map(_ :: ranges).getOrElse(ranges)
    }
  }

  def disjointRanges(lo: Int, hi: Int)(y: Int): List[Range.Inclusive] = {
    mergeAdjacent(foldAll(lo, hi)(y).sortBy(_.min), Nil)
  }

  def tuningFrequency(size: Int): BigInt = {
    val (x, y) = LazyList.iterate(0)(_ + 1)
      .map(y => (disjointRanges(0, size)(y).head.max + 1, y))
      .dropWhile(_._1 == size + 1)
      .head
    BigInt(x) * BigInt(size) + BigInt(y)
  }

  def beaconsInRange(xRange: Range.Inclusive, yValue: Int): Int = {
    input.values.toList.distinct.count { case (x, y) => xRange.contains(x) && y == yValue }
  }

  println(disjointRanges(Int.MinValue, Int.MaxValue)(2000000).map(range => range.size - beaconsInRange(range, 2000000)).sum)

  println(tuningFrequency(4000000))
}
