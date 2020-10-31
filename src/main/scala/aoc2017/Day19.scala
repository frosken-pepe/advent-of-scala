package aoc2017

import scala.io.Source

object Day19 extends App {

  val input = Source.fromFile("inputs/2017/19.txt").getLines().toIndexedSeq

  val rows = input.length

  def canGoTo(loc: (Int, Int), prev: (Int, Int)): Option[(Int, Int)] = {
    val neighs = List((loc._1 + 1, loc._2), (loc._1 - 1, loc._2), (loc._1, loc._2 + 1), (loc._1, loc._2 - 1))
      .filter { case (row, col) => row >= 0 && col >= 0 && row < rows && col < input(row).length }
      .filter(_ != prev)
      .filter(n => input(n._1)(n._2) != ' ')
    val dir = (loc._1 - prev._1, loc._2 - prev._2)
    val isTurn = input(loc._1)(loc._2) == '+'
    dir match {
      case (_, 0) if isTurn => neighs.find(n => input(n._1)(n._2) == '-')
      case (0, _) if isTurn => neighs.find(n => input(n._1)(n._2) == '|')
      case _ if !isTurn => neighs.find(n => n == (loc._1 + dir._1, loc._2 + dir._2))
    }
  }

  val curr = (0, input.head.zipWithIndex.filter(_._1 == '|').head._2)
  val prev = (curr._1 - 1, curr._2)

  val ll = LazyList.unfold((prev, curr)) {
    case (prev, curr) => canGoTo(curr, prev).map(n => (input(n._1)(n._2), (curr, n)))
  }

  println(ll.filter(_.isLetter).mkString)
  println(ll.length + 1)
}
