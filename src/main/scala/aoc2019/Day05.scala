package aoc2019

import scala.io.Source
import scala.util.Using

object Day05 extends App {

  val input = Using(Source.fromFile("inputs/2019/05.txt"))(_.getLines().next().split(",").map(_.toInt).toVector).get

  Intcode.exec(input, 0, () => 1, o => {
    if (o != 0) println(o)
  })

  Intcode.exec(input, 0, () => 5, println)
}
