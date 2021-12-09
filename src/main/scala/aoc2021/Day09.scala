package aoc2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day09 extends App {

  val input = Using(Source.fromFile("inputs/2021/09.txt"))(_.getLines()
    .map(_.map(ch => s"$ch".toInt).toList).toList
  ).get

  val width = input.head.length
  val height = input.length

  def heightAt(p: (Int, Int)): Int = p match {
    case (i, j) if i < 0 || i >= height || j < 0 || j >= width => Int.MaxValue
    case (i, j) => input(i)(j)
  }

  def neighs(p: (Int, Int)): Set[(Int, Int)] = {
    Set((-1, 0), (1, 0), (0, -1), (0, 1)).map {
      case (i, j) => (p._1 + i, p._2 + j)
    }.filterNot(heightAt(_) == Int.MaxValue)
  }

  println((for {
    i <- 0 until height
    j <- 0 until width
    height = input(i)(j)
    risk = height + 1
    if height < neighs(i, j).map(heightAt).min
  } yield risk).sum)

  def expandBasin(basin: Set[(Int, Int)]): Set[(Int, Int)] = {
    basin ++ (for {
      p <- basin
      q <- neighs(p) if !basin(q) && heightAt(q) > heightAt(p) && heightAt(q) != 9
    } yield q)
  }

  @tailrec def findBasins(found: Set[Set[(Int, Int)]]): Set[Set[(Int, Int)]] = {
    val newBasins = found.map(expandBasin)
    if (newBasins == found) newBasins
    else findBasins(newBasins)
  }

  val initialBasins = (for {
    i <- 0 until height
    j <- 0 until width
    height = input(i)(j)
    if height < List(heightAt(i - 1, j), heightAt(i + 1, j), heightAt(i, j - 1), heightAt(i, j + 1)).min
  } yield Set((i, j))).toSet

  println(findBasins(initialBasins).toList.map(_.size).sorted.reverse.take(3).product)
}
