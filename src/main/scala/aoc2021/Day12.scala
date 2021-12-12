package aoc2021

import aoc2018.Day17.fixedPoint

import scala.io.Source
import scala.util.Using

object Day12 extends App {

  case class Cave(id: String, neighs: Set[String])

  val input = Using(Source.fromFile("inputs/2021/12.txt"))(_.getLines()
    .map { case s"$a-$b" => (a, b) }
    .toList).get

  val caveIds = input.flatMap(c => List(c._1, c._2)).distinct

  val caves: Map[String, Cave] = caveIds.map(caveId => Cave(caveId,
    (input.filter(_._1 == caveId).map(_._2) ++ input.filter(_._2 == caveId).map(_._1)).toSet
  )).map(cave => cave.id -> cave).toMap

  def isFinal(path: List[String]): Boolean = {
    path.head == "end"
  }

  def expandPath(path: List[String], allowVisitTwice: Set[String]): Set[List[String]] = {
    if (path.isEmpty) Set(List("start"))
    else if (isFinal(path)) Set(path)
    else {
      val h = path.head
      val neighs = caves(h).neighs.filter {
        caveId =>
          caveId.forall(_.isUpper) || !path.contains(caveId) ||
            allowVisitTwice.contains(caveId) && path.count(c => c == caveId) == 1
      }
      neighs.map(_ :: path)
    }
  }

  def findPaths(allowVisitTwice: Set[String])(paths: Set[List[String]]): Set[List[String]] = {
    for {
      p <- paths
      q <- expandPath(p, allowVisitTwice)
    } yield q
  }

  println(fixedPoint(findPaths(Set.empty))(Set(Nil)).size)

  def part2(allow: String): Set[List[String]] = {
    fixedPoint(findPaths(Set(allow)))(Set(Nil))
  }

  val allowVisitTwice = caveIds.toSet.filter { s =>
    s.forall(_.isLower) && s != "start" && s != "end"
  }

  println(allowVisitTwice.flatMap(part2).size)
}
