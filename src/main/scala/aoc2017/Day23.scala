package aoc2017

import scala.collection.immutable.Queue
import scala.io.Source

object Day23 extends App {

  import aoc2017.Assembly._

  val program = Source.fromFile("inputs/2017/23.txt").getLines()
    .toList.map(Instruction.apply(1)).toArray

  val run: CPU => CPU = runToHalt(program)

  println(run(CPU("C64", Map[Reg, Long]() withDefaultValue 0L, 0, 0, halt = false, None, Queue(), 0, 0)).multiplications)

  // TODO
  // println(run(CPU("C64", Map[Reg, Long](Reg("a") -> 1) withDefaultValue 0L, 0, 0, halt = false, None, Queue(), 0, 0)).memory(Reg("h")))
}
