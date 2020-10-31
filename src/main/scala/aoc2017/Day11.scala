package aoc2017

import scala.io.Source

object Day11 extends App {

  val input = Source.fromFile("inputs/2017/11.txt").getLines().next().split(",").toList

  case class Pos(x: Int, y: Int) {
    def hexDist: Int = (x.abs + y.abs) / 2
  }

  def walk(dir: String, cur: Pos): Pos = dir match {
    case "n" => cur.copy(y = cur.y - 2)
    case "ne" => cur.copy(y = cur.y - 1, x = cur.x + 1)
    case "se" => cur.copy(y = cur.y + 1, x = cur.x + 1)
    case "s" => cur.copy(y = cur.y + 2)
    case "sw" => cur.copy(y = cur.y + 1, x = cur.x - 1)
    case "nw" => cur.copy(y = cur.y - 1, x = cur.x - 1)
  }

  val path = input.scanLeft(Pos(0, 0)) {
    case (cur, step) => walk(step, cur)
  }

  println(path.last.hexDist)

  println(path.map(_.hexDist).max)
}
