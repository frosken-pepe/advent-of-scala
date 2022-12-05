package aoc2021

import scala.io.Source
import scala.util.Using

object Day20 extends App {

  type Algorithm = String
  type Image = Set[(Int, Int)]

  val (algo, initialImage): (Algorithm, Image) = Using(Source.fromFile("inputs/2021/20.txt")) { s =>
    val r = s.getLines()
      .toList.mkString("\n")
      .split("\n\n").toList
    val algo = r.head.split("\n").mkString("")
    val image: Set[(Int, Int)] = r.tail.head.split("\n").zipWithIndex.flatMap {
      case (str, y) => str.zipWithIndex.flatMap {
        case ('#', x) => Some(y, x)
        case _ => None
      }
    }.toSet
    (algo, image)
  }.get

  def neighs2(y: Int, x: Int): Set[(Int, Int)] = {
    for {
      _ <- Set(())
      j <- y - 2 to y + 2
      i <- x - 2 to x + 2
    } yield (j, i)
  }

  type Rule = (Int, Int) => Boolean

  def rule(rule: Rule, y: Int, x: Int): Boolean = {
    val str = for {
      j <- y - 1 to y + 1
      i <- x - 1 to x + 1
      r = if (rule(j, i)) 1 else 0
    } yield r
    algo(Integer.parseInt(str.mkString(""), 2)) == '#'
  }

  def iterate2(image: Image): Image = {
    val r1: Rule = (j, i) => image(j, i)
    val r2: Rule = (j, i) => rule(r1, j, i)
    for {(y, x) <- image.flatMap(yx => neighs2(yx._1, yx._2)) if rule(r2, y, x)} yield (y, x)
  }

  println(LazyList.iterate(initialImage)(iterate2).drop(1).head.size)
  println(LazyList.iterate(initialImage)(iterate2).drop(25).head.size)
}
