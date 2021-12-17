package aoc2021

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day15 extends App {

  val input = Using(Source.fromFile("inputs/2021/15.txt"))(_.getLines()
    .map(_.map(ch => s"$ch".toInt).toList).toList).get

  val height = 5 * input.length
  val width = 5 * input.head.length

  val origHeight = height / 5
  val origWidth = width / 5

  @tailrec def bounded(risk: Int): Int = risk match {
    case r if r >= 1 && r <= 9 => r
    case r => bounded(r - 9)
  }

  def calcRisk(row: Int, col: Int): Int = {
    if (row >= origHeight) bounded(1 + calcRisk(row - origHeight, col))
    else if (col >= origWidth) bounded(1 + calcRisk(row, col - origWidth))
    else input(row)(col)
  }

  case class Point2D(row: Int, col: Int) {
    lazy val neighs: Set[Point2D] = (for {
      d <- Set((-1, 0), (1, 0), (0, -1), (0, 1))
      neigh = (row + d._1, col + d._2)
    } yield neigh)
      .filter { case (r, c) => r >= 0 && c >= 0 && r < height && c < width }
      .map { case (r, c) => Point2D(r, c) }

    val risk: Int = calcRisk(row, col)

    val isFinal: Boolean = col == width - 1 && row == height - 1

    override def equals(obj: Any): Boolean = {
      val pt = obj.asInstanceOf[Point2D]
      pt.row == row && pt.col == col
    }

    override def hashCode(): Int = 31 * row + col
  }

  def dijkstra(neighs: Point2D => Set[Point2D]): Int = {
    val ordering = new Ordering[(Point2D, Int)] {
      override def compare(x: (Point2D, Int), y: (Point2D, Int)): Int =
        Integer.compare(y._2, x._2)
    }
    val visited = mutable.Set[Point2D]()
    val pq = mutable.PriorityQueue[(Point2D, Int)]()(ordering)
    val risks = mutable.Map[Point2D, Int]()
    pq.enqueue((Point2D(0, 0), 0))
    risks(Point2D(0, 0)) = 0
    while (pq.nonEmpty) {
      val (v, _) = pq.dequeue()
      val risk = risks(v)
      visited.add(v)
      for {w <- neighs(v)} {
        if (!visited(w)) {
          val newRisk = risk + w.risk
          if (newRisk < risks.getOrElse(w, Int.MaxValue)) {
            risks(w) = newRisk
            pq.enqueue((w, newRisk))
          }
        }
      }
    }
    risks(Point2D(risks.keys.map(_.row).max, risks.keys.map(_.col).max))
  }

  println(dijkstra(_.neighs.filter(p => p.row < origHeight && p.col < origWidth)))
  println(dijkstra(_.neighs))
}
