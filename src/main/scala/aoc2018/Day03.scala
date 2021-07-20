package aoc2018

import scala.io.Source
import scala.util.Using

object Day03 extends App {

  case class Claim(id: Int, x: Int, y: Int, w: Int, h: Int) {

    private val xRange = x until x + w
    private val yRange = y until y + h

    val squares: Set[(Int, Int)] = (for {i <- xRange; j <- yRange} yield (i, j)).toSet

    private def overlapRange(a: Range, b: Range) = (a intersect b).nonEmpty

    def overlaps(other: Claim): Boolean = overlapRange(xRange, other.xRange) && overlapRange(yRange, other.yRange)
  }

  val claim = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  val claims = Using(Source.fromFile("inputs/2018/03.txt"))(_.getLines()
    .map { case claim(id, x, y, w, h) => Claim(id.toInt, x.toInt, y.toInt, w.toInt, h.toInt) }
    .toList).get

  val counts = claims.foldLeft(Map[(Int, Int), Int]()) {
    case (acc, claim) => claim.squares.foldLeft(acc) {
      case (acc2, square) => acc2.updated(square, acc2.getOrElse(square, 0) + 1)
    }
  }

  println(counts.count { case (_, v) => v > 1 })

  println(claims.filter { claim => !claims.filter(_.id != claim.id).exists(_.overlaps(claim)) }.head.id)
}
