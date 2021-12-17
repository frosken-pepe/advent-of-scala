package aoc2021

import scala.io.Source
import scala.util.Using

object Day17 extends App {

  val input = Using(Source.fromFile("inputs/2021/17.txt"))(_.getLines()
    .next()).get

  val targetArea = input match {
    case s"target area: x=$xmin..$xmax, y=$ymin..$ymax" => (xmin.toInt, xmax.toInt, ymin.toInt, ymax.toInt)
  }

  val (xmin, xmax, ymin, ymax) = targetArea

  case class Probe(x: Int, y: Int, vx: Int, vy: Int)

  def update(probe: Probe): Probe = {
    val x = probe.x + probe.vx
    val y = probe.y + probe.vy
    val vx = if (probe.vx > 0) probe.vx - 1
    else if (probe.vx == 0) 0
    else probe.vx + 1
    val vy = probe.vy - 1
    Probe(x, y, vx, vy)
  }

  def shoot(vx: Int, vy: Int): Option[Int] = {
    val ll = LazyList.unfold((("", Int.MinValue), Probe(0, 0, vx, vy))) {
      case ((s, highest), p@Probe(x, y, _, _)) =>
        val highestPoint = math.max(highest, y)
        if (x >= xmin && x <= xmax && y >= ymin && y <= ymax)
          Some((s, highestPoint), (("hit_target", highestPoint), update(p)))
        else if (y < ymin && s == "")
          None
        else
          Some((s, highestPoint), ((s, highestPoint), update(p)))
    }
    ll.dropWhile(_._1 == "").headOption.map(_._2)
  }

  val fired = for {
    vx <- 1 until 500
    vy <- -300 until 300
    highest <- shoot(vx, vy)
  } yield (highest, (vx, vy))

  println(fired.map(_._1).max)
  println(fired.length)
}
