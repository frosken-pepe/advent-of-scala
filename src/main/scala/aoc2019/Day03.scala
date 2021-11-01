package aoc2019

import scala.io.Source
import scala.util.Using

object Day03 extends App {

  val input = Using(Source.fromFile("inputs/2019/03.txt"))(_.getLines().map(parseWire).toList).get

  case class Section(dir: Char, amt: Int)

  def parseWire(s: String): List[Section] = {
    val re = """([UDLR])(\d+)""".r
    s.split(",").toList.map {
      case re(dir, amt) => Section(dir.head, amt.toInt)
    }
  }

  def walkSection(cur: (Int, Int), section: Section): List[(Int, Int)] = {
    val oneTo: Int => List[Int] = x => (1 to x).toList
    section match {
      case Section('U', x) => oneTo(x).map(z => (cur._1, cur._2 - z))
      case Section('D', x) => oneTo(x).map(z => (cur._1, cur._2 + z))
      case Section('L', x) => oneTo(x).map(z => (cur._1 - z, cur._2))
      case Section('R', x) => oneTo(x).map(z => (cur._1 + z, cur._2))
    }
  }

  def walk(wire: List[Section]): List[(Int, Int)] = wire.foldLeft(List((0, 0))) {
    case (acc, cur) => walkSection(acc.head, cur).reverse ++ acc
  }

  val walked = input.map(walk).map(_.reverse)

  val intersections = walked.map(_.toSet).reduce(_ intersect _) -- Set((0, 0))

  val closest = intersections.minBy {
    case (a, b) => a.abs + b.abs
  }

  println(closest._1.abs + closest._2.abs)

  val stepsToReach = walked.map(_.zipWithIndex).map(_.foldLeft(Map[(Int, Int), Int]()) {
    case (acc, item) => if (acc.contains(item._1)) acc else acc.updated(item._1, item._2)
  })

  val fewest = intersections.minBy(inter => stepsToReach.map(_ (inter)).sum)

  println(stepsToReach.map(_ (fewest)).sum)
}
