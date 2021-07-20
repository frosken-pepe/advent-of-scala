package aoc2016

import scala.util.Using

object Day23 extends App {

  import AssembunnyInterpreter._

  import scala.io.Source

  val input = Using(Source.fromFile("inputs/2016/23.txt"))(_.getLines()
    .map(assembunny)
    .toList).get

  val memory = Map(
    Reg("a") -> 12L,
    Reg("b") -> 0L,
    Reg("c") -> 0L,
    Reg("d") -> 0L,
  )

  val a = Reg("a")

  println(run(memory, input).memory(a))
}
