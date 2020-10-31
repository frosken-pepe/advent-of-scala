package aoc2016

import scala.io.Source

object Day25 extends App {

  import AssembunnyInterpreter._

  val input = Source.fromFile("inputs/2016/25.txt").getLines()
    .map(assembunny)
    .toList

  val stopWhen: (CPU) => Boolean = cpu => cpu.output.length > 20

  val memory = (a: Long) => Map(
    Reg("a") -> a,
    Reg("b") -> 0L,
    Reg("c") -> 0L,
    Reg("d") -> 0L,
  )

  val answer = LazyList.iterate(5L)(_ + 1)
    .map(a => (a, run(memory(a), input, stopWhen)))
    .filter { case (_, cpu) => cpu.output.startsWith(List[Long](0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)) }
    .map(_._1)
    .head

  println(answer)
}
