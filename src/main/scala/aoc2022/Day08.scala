package aoc2022

import scala.io.Source
import scala.util.Using

object Day08 extends App {

  val trees: List[List[Int]] =
    Using(Source.fromFile("inputs/2022/08.txt"))(_.getLines().toList).get.map(_.map(ch => s"$ch".toInt).toList)

  val width = trees.head.size
  val height = trees.size

  private def isVisible(row: Int, col: Int) = {
    if (row == 0 || col == 0) true
    else if (row == height - 1 || col == width - 1) true
    else if ((0 until col).forall(c => trees(row)(c) < trees(row)(col))) true
    else if ((0 until row).forall(r => trees(r)(col) < trees(row)(col))) true
    else if ((col + 1 until width).forall(c => trees(row)(c) < trees(row)(col))) true
    else if ((row + 1 until height).forall(r => trees(r)(col) < trees(row)(col))) true
    else false
  }

  println(
    (for {
      row <- 0 until height
      col <- 0 until width
      if isVisible(row, col)
    } yield (row, col)).size)

  private def scenicScore(row: Int, col: Int) = {
    val up = LazyList.iterate(row - 1)(_ - 1).takeWhile(r => r > 0 && trees(r)(col) < trees(row)(col)).size + 1
    val down = LazyList.iterate(row + 1)(_ + 1).takeWhile(r => r < height - 1 && trees(r)(col) < trees(row)(col)).size + 1
    val left = LazyList.iterate(col - 1)(_ - 1).takeWhile(c => c > 0 && trees(row)(c) < trees(row)(col)).size + 1
    val right = LazyList.iterate(col + 1)(_ + 1).takeWhile(c => c < width - 1 && trees(row)(c) < trees(row)(col)).size + 1
    up * down * left * right
  }

  println(
    (for {
      row <- 1 until height - 1
      col <- 1 until width - 1
    } yield scenicScore(row, col)).max)
}
