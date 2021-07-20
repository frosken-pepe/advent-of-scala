package aoc2018

import scala.io.Source
import scala.util.Using

object Day10 extends App {

  type Vec2 = (Long, Long)

  case class Point(p: Vec2, v: Vec2) {
    def move: Point = copy(p = (p._1 + v._1, p._2 + v._2))
  }

  object Input {
    private val re = """position=<\s*(-?\d+),\s*(-?\d+)>\s*velocity=<\s*(-?\d+),\s*(-?\d+)>""".r
    val input: List[Point] = Using(Source.fromFile("inputs/2018/10.txt"))(_.getLines().map {
      case re(px, py, vx, vy) => Point((px.toLong, py.toLong), (vx.toLong, vy.toLong))
    }.toList).get
  }

  def boundingBoxArea(points: List[Point]): Long = {
    val x = points.map(_.p._1)
    val y = points.map(_.p._2)
    (x.max - x.min) * (y.max - y.min)
  }

  def viz(points: List[Point]): Unit = {
    val x = points.map(_.p._1)
    val y = points.map(_.p._2)
    for {
      y <- y.min to y.max
      strRow = (x.min to x.max).map(x => if (points.exists(p => p.p == (x, y))) '#' else ' ').mkString
    } println(strRow)
  }

  val bb0 = boundingBoxArea(Input.input)

  val (t, finalPoints) = LazyList.unfold((0, Input.input, bb0)) { case (t, points, bb) =>
    val nextPoints = points.map(_.move)
    val newBB = boundingBoxArea(nextPoints)
    if (newBB < bb) {
      Some(((t + 1, nextPoints), (t + 1, nextPoints, newBB)))
    } else None
  }.last

  viz(finalPoints)

  println(t)
}
