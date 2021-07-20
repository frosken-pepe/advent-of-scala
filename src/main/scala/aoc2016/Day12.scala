package aoc2016

import scala.util.Using

object Day12 extends App {

  import scala.io.Source
  import AssembunnyInterpreter._

  val input = Using(Source.fromFile("inputs/2016/12.txt"))(_.getLines()
    .map(assembunny)
    .toList).get

  val memory = Map(
    Reg("a") -> 0L,
    Reg("b") -> 0L,
    Reg("c") -> 0L,
    Reg("d") -> 0L,
  )

  val a = Reg("a")

  println(run(memory, input).memory(a))

  println(run(memory.updated(Reg("c"), 1L), input).memory(a))
}
