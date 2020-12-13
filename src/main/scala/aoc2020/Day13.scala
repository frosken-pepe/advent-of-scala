package aoc2020

import scala.io.Source

object Day13 extends App {

  val input = Source.fromFile("inputs/2020/13.txt").getLines().toList

  val timestamp = input.head.toInt
  val busses = input.tail.head.split(",").filter(_ != "x").map(_.toInt).toList

  println(
    LazyList.iterate(timestamp)(_ + 1).filter(ts => busses.exists(b => ts % b == 0))
      .map(ts => (ts - timestamp) * busses.filter(b => ts % b == 0).head)
      .head
  )

  val bussesP2 = input.tail.head.split(",").zipWithIndex
    .filter(_._1 != "x").map {
    case (k, v) => k.toInt -> v
  }.toList

  val m = bussesP2.map(_._1).map(_.toLong)
  val a = bussesP2.map(t => (t._1 - t._2) % t._1)
  val M = m.product
  val b = m.map(mm => BigInt(M / mm).modInverse(mm))
  val t = a.zip(b).zip(m).map { case ((a, b), m) => a * b * (M / m) }.sum % M

  println(t)
}
