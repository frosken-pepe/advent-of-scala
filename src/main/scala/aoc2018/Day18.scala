package aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day18 extends App {

  sealed trait Acre

  case object Open extends Acre

  case object Trees extends Acre

  case object Lumberyard extends Acre

  object Acre {
    def apply(ch: Char): Acre = ch match {
      case '|' => Trees
      case '.' => Open
      case '#' => Lumberyard
    }
  }

  case class Vec2(x: Int, y: Int) {
    def neighs: Set[Vec2] = (for {
      dx <- -1 to 1
      dy <- -1 to 1 if !(dx == 0 && dy == 0)
      nx = x + dx if nx >= 0 && nx < 50
      ny = y + dy if ny >= 0 && ny < 50
    } yield Vec2(nx, ny)).toSet
  }

  case class Area(map: Map[Vec2, Acre]) {
    def next: Area = Area(map.map {
      case (loc, acre) if acre == Open && loc.neighs.count(map(_) == Trees) >= 3 => loc -> Trees
      case (loc, acre) if acre == Trees && loc.neighs.count(map(_) == Lumberyard) >= 3 => loc -> Lumberyard
      case (loc, acre) if acre == Lumberyard && (loc.neighs.count(map(_) == Lumberyard) < 1 || loc.neighs.count(map(_) == Trees) < 1) => loc -> Open
      case x => x
    })

    def resourceValue: Int = map.count { case (_, acre) => acre == Trees } * map.count { case (_, acre) => acre == Lumberyard }
  }

  val input = Area(Source.fromFile("inputs/2018/18.txt").getLines().zipWithIndex.flatMap {
    case (line, y) => line.zipWithIndex.map {
      case (ch, x) => Vec2(x, y) -> Acre(ch)
    }
  }.toMap)

  @tailrec def evolve(todo: Int, area: Area): Area = {
    if (todo == 0) area
    else evolve(todo - 1, area.next)
  }

  println(evolve(10, input).resourceValue)

  @tailrec def findPeriod(cur: Area, seen: Set[Area], history: List[Area]): (Int, List[Int]) = {
    if (seen.contains(cur)) (history.indexOf(cur) + 1, history.map(_.resourceValue))
    else findPeriod(cur.next, seen + cur, cur :: history)
  }

  val target = 1000000000
  val (period, history) = findPeriod(input, Set(), Nil)

  println(history(period - ((target - history.length + 1) % period)))
}
