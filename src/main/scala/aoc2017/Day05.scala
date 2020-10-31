package aoc2017

import scala.io.Source

object Day05 extends App {

  val input = Source.fromFile("inputs/2017/05.txt")
    .getLines().map(_.toInt).zipWithIndex.map(p => p._2 -> p._1).toMap

  def countSteps(update: Int => Int) = LazyList.unfold((0, input)) {
    case (idx, map) if idx >= 0 && idx < map.size =>
      Some((1, (idx + map(idx), map.updated(idx, update(map(idx))))))
    case _ => None
  }.foldLeft(0) { case (acc, item) => acc + item }

  println(countSteps(_ + 1))
  println(countSteps(x => if (x >= 3) x - 1 else x + 1))
}
