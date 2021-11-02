package aoc2019

import scala.annotation.tailrec

object Intcode {

  object Modes {
    val POSITION = 0
    val IMMEDIATE = 1
  }

  object Opcodes {
    val HALT = 99
    val ADD = 1
    val MULT = 2
    val INPUT = 3
    val OUTPUT = 4
    val JUMP_IF_TRUE = 5
    val JUMP_IF_FALSE = 6
    val LESS_THAN = 7
    val EQUALS = 8
  }

  @tailrec def exec(program: Vector[Int],
                    ip: Int = 0,
                    input: () => Int = () => throw new IllegalArgumentException("missing input"),
                    output: Int => Unit = _ => throw new IllegalArgumentException("missing output")): Vector[Int] = {
    import Opcodes._
    val (opcode, mode1, mode2, mode3) = parseInstruction(program(ip))
    if (opcode == HALT) program
    else {
      val op1 = valueOf(program, ip + 1, mode1)
      val op2 = valueOf(program, ip + 2, mode2)
      val store = valueOf(program, ip + 3, mode3)
      if (opcode == ADD) exec(program.updated(store, program(op1) + program(op2)), ip + 4, input, output)
      else if (opcode == MULT) exec(program.updated(store, program(op1) * program(op2)), ip + 4, input, output)
      else if (opcode == INPUT) exec(program.updated(store, input()), ip + 2, input, output)
      else if (opcode == OUTPUT) {
        output(program(op1))
        exec(program, ip + 2, input, output)
      } else if (opcode == JUMP_IF_TRUE) {
        if (program(op1) != 0) exec(program, program(op2), input, output)
        else exec(program, ip + 3, input, output)
      } else if (opcode == JUMP_IF_FALSE) {
        if (program(op1) == 0) exec(program, program(op2), input, output)
        else exec(program, ip + 3, input, output)
      } else if (opcode == LESS_THAN) {
        exec(program.updated(store, if (program(op1) < program(op2)) 1 else 0), ip + 4, input, output)
      } else if (opcode == EQUALS) {
        exec(program.updated(store, if (program(op1) == program(op2)) 1 else 0), ip + 4, input, output)
      }
      else throw new IllegalStateException(s"? opcode $opcode")
    }
  }

  def valueOf(program: Vector[Int], ip: Int, mode: Int): Int = {
    import Modes._
    if (mode == POSITION) program(ip)
    else if (mode == IMMEDIATE) ip
    else throw new IllegalArgumentException(s"? mode $mode")
  }

  def parseInstruction(instruction: Int): (Int, Int, Int, Int) = {
    val opcode = instruction % 100
    val mode1 = (instruction / 100) % 2
    val mode2 = (instruction / 1000) % 2
    val mode3 = (instruction / 10000) % 2
    (opcode, mode1, mode2, mode3)
  }
}
