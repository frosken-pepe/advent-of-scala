package aoc2020

import scala.annotation.tailrec
import scala.io.Source

object Day24 extends App {

  val input = Source.fromFile("inputs/2020/24.txt").getLines().toList

  def tile(s: String): Option[(String, String)] = {
    if (s.startsWith("ne")) Some("ne", s.drop(2))
    else if (s.startsWith("nw")) Some("nw", s.drop(2))
    else if (s.startsWith("sw")) Some("sw", s.drop(2))
    else if (s.startsWith("se")) Some("se", s.drop(2))
    else if (s.startsWith("e")) Some("e", s.drop(1))
    else if (s.startsWith("w")) Some("w", s.drop(1))
    else None
  }

  @tailrec def parseLine(line: String, acc: List[String]): List[String] = {
    tile(line) match {
      case Some((tile, remain)) => parseLine(remain, tile :: acc)
      case None => acc.reverse
    }
  }

  case class Vec2(x: Int, y: Int)

  def walk(dir: String, cur: Vec2): Vec2 = dir match {
    case "e" => cur.copy(x = cur.x + 2)
    case "w" => cur.copy(x = cur.x - 2)
    case "ne" => cur.copy(y = cur.y - 1, x = cur.x + 1)
    case "se" => cur.copy(y = cur.y + 1, x = cur.x + 1)
    case "sw" => cur.copy(y = cur.y + 1, x = cur.x - 1)
    case "nw" => cur.copy(y = cur.y - 1, x = cur.x - 1)
  }

  val initBlack = Set[Vec2]()
  val origin = Vec2(0, 0)

  val day1 = input.foldLeft(initBlack) {
    case (acc, line) =>
      val pos = parseLine(line, Nil).foldLeft(origin) {
        case (cur, step) => walk(step, cur)
      }
      if (acc.contains(pos)) acc - pos else acc + pos
  }

  def neighs(pos: Vec2): Set[Vec2] = {
    Set("e", "se", "sw", "w", "nw", "ne").map(dir => walk(dir, pos))
  }

  def blackNeighs(pos: Vec2, tiles: Set[Vec2]): Int = {
    neighs(pos).count(tiles.contains)
  }

  def next(isBlack: Boolean, blackNeighs: Int) = (isBlack, blackNeighs) match {
    case (true, 1) => true
    case (true, 2) => true
    case (true, _) => false
    case (false, 2) => true
    case (false, _) => false
  }

  def evolve(floor: Set[Vec2]): Set[Vec2] = {
    val consider = floor.flatMap(neighs) ++ floor
    consider.filter { pos =>
      val isBlack = floor.contains(pos)
      val b = blackNeighs(pos, floor)
      next(isBlack, b)
    }
  }

  println(day1.size)

  println(
    LazyList.iterate(day1)(evolve)
      .map(_.size)
      .drop(100)
      .head)

}
