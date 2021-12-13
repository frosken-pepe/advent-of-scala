package aoc2021

import scala.io.Source
import scala.util.Using

object Day13 extends App {

  val input = Using(Source.fromFile("inputs/2021/13.txt"))(_.getLines().toList).get

  val inputCoords = input.takeWhile(_.nonEmpty).map {
    case s"$a,$b" => (a.toInt, b.toInt)
  }.toSet

  val folds = input.drop(inputCoords.size + 1).map {
    case s"fold along $c=$i" => (c, i.toInt)
  }

  def foldX(coords: Set[(Int, Int)], line: Int): Set[(Int, Int)] = {
    coords.map {
      case (x, y) if x <= line => (x, y)
      case (x, y) => (2 * line - x, y)
    }
  }

  def foldY(coords: Set[(Int, Int)], line: Int): Set[(Int, Int)] = {
    coords.map {
      case (x, y) if y <= line => (x, y)
      case (x, y) => (x, 2 * line - y)
    }
  }

  def draw(coords: Set[(Int, Int)]): Unit = {
    for {y <- coords.map(_._2).min to coords.map(_._2).max} {
      for {x <- coords.map(_._1).min to coords.map(_._1).max} {
        print(if (coords(x, y)) '#' else ' ')
      }
      println()
    }
  }

  def foldAll(coords: Set[(Int, Int)], folds: List[(String, Int)]): Set[(Int, Int)] = {
    folds.foldLeft(coords) {
      case (acc, ("x", x)) => foldX(acc, x)
      case (acc, ("y", y)) => foldY(acc, y)
    }
  }

  println(foldAll(inputCoords, folds.take(1)).size)

  draw(foldAll(inputCoords, folds))
}
