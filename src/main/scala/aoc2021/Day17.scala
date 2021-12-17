package aoc2021

import scala.io.Source
import scala.util.Using

object Day17 extends App {

  val (xmin, xmax, ymin, ymax) = Using(Source.fromFile("inputs/2021/17.txt"))(_.getLines().next() match {
    case s"target area: x=$xmin..$xmax, y=$ymin..$ymax" => (xmin.toInt, xmax.toInt, ymin.toInt, ymax.toInt)
  }).get

  case class Probe(x: Int, y: Int, vx: Int, vy: Int, hitTarget: Boolean, highestPoint: Int)

  def update(probe: Probe): Probe = {
    val newProbe = Probe(probe.x + probe.vx, probe.y + probe.vy, probe.vx - probe.vx.sign, probe.vy - 1,
      probe.hitTarget, probe.highestPoint)
    newProbe
      .copy(hitTarget = probe.hitTarget || newProbe.x >= xmin && newProbe.x <= xmax && newProbe.y >= ymin && newProbe.y <= ymax)
      .copy(highestPoint = math.max(probe.highestPoint, newProbe.y))
  }

  def shoot(vx: Int, vy: Int): Option[Int] = {
    LazyList.unfold(Probe(0, 0, vx, vy, hitTarget = false, Int.MinValue)) {
      case p@Probe(x, y, _, _, _, _) =>
        if (y < ymin || x > xmax) None
        else Some((p, update(p)))
    }.dropWhile(!_.hitTarget).headOption.map(_.highestPoint)
  }

  val fired = for {
    vx <- 1 until 500
    vy <- -300 until 300
    highest <- shoot(vx, vy)
  } yield (highest, (vx, vy))

  println(fired.map(_._1).max)
  println(fired.length)
}
