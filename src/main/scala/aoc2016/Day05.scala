package aoc2016

import aoc2015.Day04.md5

import scala.io.Source

object Day05 extends App {

  val doorId = Source.fromFile("inputs/2016/05.txt").getLines().next()

  println(
    LazyList.iterate(0)(_ + 1).map(i => md5(doorId + i)).filter(_.startsWith("00000"))
      .map(_ (5)).take(8).mkString.toLowerCase
  )

  println(
    LazyList.iterate(0)(_ + 1)
      .map(i => md5(doorId + i))
      .filter(_.startsWith("00000"))
      .map(hash => (hash(5), hash(6)))
      .filter(pair => pair._1 >= '0' && pair._1 <= '7')
      .map(pair => (s"${pair._1}".toInt, pair._2))
      .scanLeft(Map[Int, Char]()) {
        case (map, (pos, ch)) if !map.contains(pos) => map.updated(pos, ch)
        case (map, _) => map
      }
      .dropWhile(map => map.size < 8)
      .head
      .toList
      .sortBy(_._1)
      .map(_._2)
      .mkString
      .toLowerCase)
}
