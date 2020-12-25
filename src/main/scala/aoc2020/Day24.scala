package aoc2020

import scala.annotation.tailrec
import scala.io.Source

object Day24 extends App {

  def tile(s: String): Option[(String, String)] = s.headOption.map {
    case 'e' | 'w' => s.splitAt(1)
    case 'n' | 's' => s.splitAt(2)
  }

  @tailrec def parseLine(line: String, acc: List[String] = Nil): List[String] = tile(line) match {
    case Some((tile, remain)) => parseLine(remain, tile :: acc)
    case None => acc.reverse
  }

  val input = Source.fromFile("inputs/2020/24.txt").getLines().map(parseLine(_)).toList

  case class Vec2(x: Int, y: Int) {
    def step(dir: String): Vec2 = dir match {
      case "e" => copy(x = x + 2)
      case "w" => copy(x = x - 2)
      case "ne" => copy(y = y - 1, x = x + 1)
      case "se" => copy(y = y + 1, x = x + 1)
      case "sw" => copy(y = y + 1, x = x - 1)
      case "nw" => copy(y = y - 1, x = x - 1)
    }

    lazy val neighs: Set[Vec2] = {
      Set("e", "se", "sw", "w", "nw", "ne").map(step)
    }
  }

  val initBlack = Set[Vec2]()
  val origin = Vec2(0, 0)

  def walk(steps: List[String], start: Vec2): Vec2 = steps.foldLeft(start) {
    case (cur, step) => cur.step(step)
  }

  val day1 = input.map(walk(_, origin)).foldLeft(initBlack) {
    case (acc, pos) => if (acc.contains(pos)) acc - pos else acc + pos
  }

  def blackNeighs(pos: Vec2, tiles: Set[Vec2]): Int = {
    pos.neighs.count(tiles.contains)
  }

  def next(isBlack: Boolean, blackNeighs: Int) = (isBlack, blackNeighs) match {
    case (true, 1) => true
    case (true, 2) => true
    case (true, _) => false
    case (false, 2) => true
    case (false, _) => false
  }

  def evolve(floor: Set[Vec2]): Set[Vec2] = {
    (floor.flatMap(_.neighs) ++ floor).filter { pos =>
      next(floor.contains(pos), blackNeighs(pos, floor))
    }
  }

  println(day1.size)

  println(
    LazyList.iterate(day1)(evolve)
      .map(_.size)
      .drop(100)
      .head)
}
