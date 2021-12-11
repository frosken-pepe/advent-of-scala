package aoc2021

import aoc2018.Day17.fixedPoint

import scala.io.Source
import scala.util.Using

object Day11 extends App {

  val input = Using(Source.fromFile("inputs/2021/11.txt"))(_.getLines()
    .map(s => s.map(ch => s"$ch".toInt).toVector)
    .toVector).get

  val height = input.size
  val width = input.head.size

  def neighs(p: (Int, Int)): Set[(Int, Int)] = {
    for {
      _ <- Set(())
      dx <- -1 to 1
      dy <- -1 to 1 if dy != 0 || dx != 0
      x = p._2 + dx if x >= 0 && x < width
      y = p._1 + dy if y >= 0 && y < height
    } yield (y, x)
  }

  def substep(octopi: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    octopi.zipWithIndex.map {
      case (row, j) => row.zipWithIndex.map {
        case (oct, _) if oct >= 10 || oct == 0 => 0
        case (oct, i) => oct + neighs(j, i).count(p => octopi(p._1)(p._2) >= 10)
      }
    }
  }

  def step(octopi: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    fixedPoint(substep)(octopi.map(_.map(_ + 1)))
  }

  println(
    LazyList.iterate(input)(step).drop(1).map(_.flatten.count(_ == 0)).take(100).sum)

  println(
    LazyList.iterate(input)(step).zipWithIndex.dropWhile {
      case (octopi, _) => octopi.exists(_.exists(_ != 0))
    }.head._2)
}
