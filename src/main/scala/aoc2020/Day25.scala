package aoc2020

import scala.io.Source

object Day25 extends App {

  val input = Source.fromFile("inputs/2020/25.txt").getLines().map(_.toLong).toList

  val m = 20201227L

  def transform(a: Long, x: Long): Long = {
    // a^x (mod m)
    BigInt(a).modPow(x, m).longValue
  }

  def discreteLog(a: Long, p: Long): Long = {
    // find x s.t. a^x = p (mod m)
    LazyList.iterate(1L)(z => (z * a) % m).zipWithIndex.filter(_._1 == p).map(_._2).head
  }

  val cardPubKey = input.head
  val doorPubKey = input.tail.head

  println(transform(doorPubKey, discreteLog(7, cardPubKey)))
}
