package aoc2016

import scala.io.Source

object Day06 extends App {

  val input = Source.fromFile("inputs/2016/06.txt").getLines().toList

  val messageLen = input.head.length
  val listOfEmptyMaps = (0 until messageLen).map(_ => Map[Char, Int]()).toList

  def decode(f: List[(Char, Int)] => List[(Char, Int)]): String = {
    input.foldLeft(listOfEmptyMaps) {
      case (maps, message) => maps.zip(message.toList).map {
        case (map, c) => map.updated(c, map.getOrElse(c, 0) + 1)
      }
    }.map(z => f(z.toList.sortBy(_._2)).head._1).mkString
  }

  println(decode(_.reverse))
  println(decode(identity))
}
