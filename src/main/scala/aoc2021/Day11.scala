package aoc2021

import aoc2018.Day17.fixedPoint

import scala.io.Source
import scala.util.Using

object Day11 extends App {

  val input = Using(Source.fromFile("inputs/2021/11.txt"))(_.getLines()
    .map(s => s.map(ch => s"$ch".toInt).toList)
    .toList).get

  val height = input.size
  val width = input.head.size

  def neighs(p: (Int, Int)): Set[(Int, Int)] = {
    Set((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)).map {
      case (j, i) => (p._1 + j, p._2 + i)
    }.filter {
      case (j, i) if j >= 0 && j < height && i >= 0 && i < width => true
      case _ => false
    }
  }

  def substep(octopi: List[List[Int]]): List[List[Int]] = {
    val flashed = octopi.zipWithIndex.flatMap {
      case (row, j) => row.zipWithIndex.flatMap {
        case (10, i) => Some(j, i)
        case _ => None
      }
    }.toSet
    octopi.zipWithIndex.map {
      case (row, j) => row.zipWithIndex.map {
        case (oct, i) if flashed(j, i) || oct == 0 => 0
        case (oct, i) => math.min(10, oct + neighs(j, i).intersect(flashed).size)
      }
    }
  }

  def step(octopi: List[List[Int]]): List[List[Int]] = {
    fixedPoint(substep)(octopi.map(_.map(_ + 1)))
  }

  println(
    LazyList.iterate(input)(step).drop(1).map(_.flatten.count(_ == 0)).take(100).sum)

  println(
    LazyList.iterate(input)(step).zipWithIndex.dropWhile {
      case (octopi, _) => octopi.flatten.toSet != Set(0)
    }.head._2)
}
