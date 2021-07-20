package aoc2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day20 extends App {

  val input = Using(Source.fromFile("inputs/2016/20.txt"))(_.getLines()
    .map(_.split("-").map(_.toLong).toList)
    .map { case from :: to :: Nil => (from, to) }
    .toList).get

  val candidates = List(0L) ++ input.map(_._2 + 1)

  def isAllowed(ip: Long): Boolean =
    ip <= 4294967295L && !input.exists(range => ip >= range._1 && ip <= range._2)

  println(candidates.filter(isAllowed).min)

  @tailrec def count(candidates: List[Long], acc: Int = 0): Int =
    if (candidates.isEmpty) acc
    else {
      val allowed = candidates.filter(isAllowed)
      count(allowed.map(_ + 1), acc + allowed.size)
    }

  println(count(candidates))
}
