package aoc2016

object Day23 extends App {

  import AssembunnyInterpreter._

  import scala.io.Source

  val input = Source.fromFile("inputs/2016/23.txt").getLines()
    .map(assembunny)
    .toList

  val memory = Map(
    Reg("a") -> 12L,
    Reg("b") -> 0L,
    Reg("c") -> 0L,
    Reg("d") -> 0L,
  )

  val a = Reg("a")

  println(run(memory, input).memory(a))
}
