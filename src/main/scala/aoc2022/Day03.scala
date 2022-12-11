package aoc2022

import scala.io.Source
import scala.util.Using

object Day03 extends App {

  private val input: List[(String, String)] = Using(Source.fromFile("inputs/2022/03.txt"))(_.getLines()
    .map(line => line.splitAt(line.length / 2))
    .toList).get

  private def priority(ch: Char): Int = ch match {
    case x if 'a' <= x && x <= 'z' => x - 'a' + 1
    case x if 'A' <= x && x <= 'Z' => x - 'A' + 27
  }

  println(input.map { case (c1, c2) => c1.intersect(c2).map(priority).distinct.sum }.sum)

  def common(rucksacks: List[String]): Char = rucksacks.tail.foldLeft(rucksacks.head) {
    case (acc, r) => acc intersect r
  }.head

  println(input.map { case (a, b) => s"$a$b" }.sliding(3, 3).toList.map(common).map(priority).sum)
}
