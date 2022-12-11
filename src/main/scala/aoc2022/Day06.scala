package aoc2022

import scala.io.Source
import scala.util.Using

object Day06 extends App {

  private val input: String = Using(Source.fromFile("inputs/2022/06.txt"))(_.getLines().next()).get

  def marker(size: Int): Int = {
    input.sliding(size, 1).toList.zipWithIndex.dropWhile(_._1.toSet.size != size).map(_._2).head + size
  }

  println(marker(4))

  println(marker(14))
}
