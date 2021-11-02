package aoc2019

import scala.io.Source
import scala.util.Using

object Day02 extends App {

  val input = Using(Source.fromFile("inputs/2019/02.txt"))(
    _.getLines().next().split(",").map(_.toInt).toVector).get

  def execWithInput(noun: Int, verb: Int): Int = {
    Intcode.exec(input.updated(1, noun).updated(2, verb))(0)
  }

  println(execWithInput(12, 2))

  val inputs = for {
    noun <- LazyList.iterate(0)(_ + 1)
    verb <- 0 to noun
  } yield (noun, verb)

  println(
    inputs.find(i => execWithInput(i._1, i._2) == 19690720)
      .map(i => 100 * i._1 + i._2).get)
}
