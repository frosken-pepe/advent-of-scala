package aoc2017

import scala.io.Source
import scala.util.Using

object Day13 extends App {

  val input = Using(Source.fromFile("inputs/2017/13.txt"))(_.getLines()
    .map(line => line.split(": ").map(_.toInt).toList)
    .map { case k :: v :: Nil => (k, 2 * v - 2) }
    .toMap).get

  def severity(delay: Int): Int = (for {
    (depth, dblRange) <- input if (delay + depth) % dblRange == 0
    severity = depth * (dblRange + 2) / 2
  } yield severity).sum

  println(severity(0))

  println(LazyList.iterate(0)(_ + 1).filter(t => input.forall {
    case (d, r) => (t + d) % r != 0
  }).head)
}
