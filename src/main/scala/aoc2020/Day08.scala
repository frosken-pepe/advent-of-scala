package aoc2020

import scala.io.Source
import scala.util.Using

object Day08 extends App {

  case class CPU(accumulator: Long, pc: Int, halt: Boolean, visited: Set[Int], terminatedNormally: Boolean) {

    def next: CPU = copy(pc = pc + 1)

    def exec(program: IndexedSeq[Instruction]): CPU = {
      if (visited contains pc) copy(halt = true)
      else if (pc >= 0 && pc < program.length) program(pc).exec(this.copy(visited = this.visited + pc))
      else copy(halt = true, terminatedNormally = true)
    }
  }

  sealed abstract class Instruction {
    def swap: Instruction

    def exec(cpu: CPU): CPU
  }

  object Instruction {
    private val nop = """nop ([+-]\d+)""".r
    private val acc = """acc ([+-]\d+)""".r
    private val jmp = """jmp ([+-]\d+)""".r

    def apply(s: String): Instruction = s match {
      case nop(a) => NOP(a.toInt)
      case acc(a) => ACC(a.toInt)
      case jmp(a) => JMP(a.toInt)
    }
  }

  case class NOP(a: Int) extends Instruction {
    override def swap: Instruction = JMP(a)

    override def exec(cpu: CPU): CPU = cpu.next
  }

  case class ACC(a: Int) extends Instruction {
    override def swap: Instruction = this

    override def exec(cpu: CPU): CPU = cpu.next.copy(accumulator = cpu.accumulator + a)
  }

  case class JMP(a: Int) extends Instruction {
    override def swap: Instruction = NOP(a)

    override def exec(cpu: CPU): CPU = cpu.copy(pc = cpu.pc + a)
  }

  def runToHalt(program: Array[Instruction])(cpu: CPU): CPU = {
    LazyList.iterate(cpu) { cpu =>
      cpu.exec(program)
    }.dropWhile(cpu => !cpu.halt).head
  }

  val program = Using(Source.fromFile("inputs/2020/08.txt"))(_.getLines()
    .map { s => Instruction.apply(s) }
    .toArray).get

  val cpu = CPU(0, 0, halt = false, Set(), terminatedNormally = false)

  println(runToHalt(program)(cpu).accumulator)

  def swapInstruction(program: Array[Instruction], idx: Int): Array[Instruction] = {
    program.zipWithIndex.map {
      case (instr, `idx`) => instr.swap
      case (instr, _) => instr
    }
  }

  println(
    (for {
      idx <- program.indices
      newProgram = swapInstruction(program, idx)
      result = runToHalt(newProgram)(cpu) if result.terminatedNormally
    } yield result.accumulator).head
  )
}
