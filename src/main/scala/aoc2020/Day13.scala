package aoc2020

import scala.io.Source

object Day13 extends App {

  val input = Source.fromFile("inputs/2020/13.txt").getLines().toList

  val timestamp = input.head.toInt

  val busses = input.tail.head.split(",")
    .toList
    .zipWithIndex
    .filter(_._1 != "x")
    .map { case (k, v) => k.toInt -> v }

  val busIds = busses.map(_._1)

  println(
    LazyList.iterate(timestamp)(_ + 1)
      .map(ts => (ts, busIds.filter(b => ts % b == 0)))
      .dropWhile(_._2.isEmpty)
      .map { case (ts, id :: Nil) => (ts - timestamp) * id }
      .head
  )

  // solve system of congruences x % ms(i) = as(i)
  def chinaChinaChina(as: List[Int], ms: List[Int]): BigInt = {
    assert(ms.forall(_ > 0))
    // just make sure we have the remainders a(i) in [0..m(i)-1]
    val a = as.zip(ms).map { case (a, m) => ((a % m) + m) % m }
    val M = ms.map(m => BigInt(m)).product
    val x = ms.map(M / _)
    val b = ms.zip(x).map { case (m, x) => x modInverse m }
    a.zip(b).zip(x).map { case ((a, b), x) => a * b * x }.sum mod M
  }

  println(chinaChinaChina(busses.map(t => -t._2), busses.map(_._1)))
}
