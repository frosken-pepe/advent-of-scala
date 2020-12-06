package aoc2018

import scala.io.Source

object Day11 extends App {

  val serialNo = Source.fromFile("inputs/2018/11.txt").getLines().next().toInt

  def power(x: Int, y: Int): Int = {
    val rackID = x + 10
    (rackID * ((rackID * y) + serialNo) / 100) % 10 - 5
  }

  type Grid = IndexedSeq[IndexedSeq[Int]]

  def max1dSum(arr: IndexedSeq[Int], windowSize: Int): (Int, Int) = {
    arr.sliding(windowSize).map(_.sum).zipWithIndex.maxBy(_._1)
  }

  def helper(temp: Grid, bottom: Int, top: Int) = {
    val sums = if (bottom > 0) temp.indices.map(i => temp(top)(i) - temp(bottom - 1)(i))
    else temp.indices.map(i => temp(top)(i))
    max1dSum(sums, top - bottom + 1)
  }

  def kadane2D(grid: Grid, sizeFilter: Int => Boolean): (Int, Int, Int) = {
    val N = grid.length
    val temp: Grid = grid.scanLeft(grid.head.map(_ => 0)) {
      case (a, b) => a.zip(b).map(p => p._1 + p._2)
    }.drop(1)
    val (sz, y, (_, x)) = (for {
      bottom <- 0 until N
      top <- bottom until N
      szz = top - bottom + 1 if sizeFilter(szz)
    } yield (szz, bottom, helper(temp, bottom, top))).maxBy(_._3._1)
    (x + 1, y + 1, sz)
  }

  val grid: Grid = for {y <- 1 to 300; row = (1 to 300).map(x => power(x, y))} yield row

  val p1 = kadane2D(grid, _ == 3)
  println(s"${p1._1},${p1._2}")

  val p2 = kadane2D(grid, _ > 0)
  println(s"${p2._1},${p2._2},${p2._3}")
}
