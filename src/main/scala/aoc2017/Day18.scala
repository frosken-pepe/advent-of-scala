package aoc2017

import scala.collection.immutable.Queue
import scala.io.Source
import scala.util.Using

object Day18 extends App {

  import Assembly._

  val input = Using(Source.fromFile("inputs/2017/18.txt"))(_.getLines().toList).get

  private def part1(): Long = {
    val cpu = CPU("C64", Map[Reg, Long]() withDefaultValue 0L, 0, 0, halt = false, None, Queue(), 0, 0)
    val program = input.map(Instruction.apply(1)).toArray
    LazyList.unfold(cpu) { cpu =>
      val newCpu = cpu.exec(program)
      if (newCpu.halt) None
      else Some(newCpu.lastSound, newCpu)
    }.last
  }

  println(part1())

  private def part2(): Int = {

    val program = input.map(Instruction.apply(2)).toArray
    val run: CPU => CPU = runToHalt(program)

    val cpu0 = CPU("286", Map[Reg, Long](Reg("p") -> 0L) withDefaultValue 0L, 0, 0, halt = false, None, Queue(), 0, 0)
    val cpu1 = CPU("386", Map[Reg, Long](Reg("p") -> 1L) withDefaultValue 0L, 0, 0, halt = false, None, Queue(), 0, 0)

    val transmit: ((CPU, CPU)) => (CPU, CPU) = {
      case (sender, receiver) =>
        if (sender.send.nonEmpty) (sender.copy(send = sender.send.dequeue._2), receiver.copy(receive = Some(sender.send.dequeue._1), halt = false))
        else (sender, receiver)
    }

    val runForrestRun: ((CPU, CPU)) => (CPU, CPU) = cpus => (run(cpus._1), run(cpus._2))

    LazyList.iterate((cpu0, cpu1)) {
      case (cpu0, cpu1) => (runForrestRun andThen transmit andThen (_.swap) andThen transmit andThen (_.swap)) (cpu0, cpu1)
    }.tail.dropWhile {
      case (cpu0, cpu1) => cpu0.receive.nonEmpty || cpu1.receive.nonEmpty
    }.head._2.sent
  }

  println(part2())
}
