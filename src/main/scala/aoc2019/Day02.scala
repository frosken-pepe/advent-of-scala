package aoc2019

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day02 extends App {

  val input = Using(Source.fromFile("inputs/2019/02.txt"))(
    _.getLines().next().split(",").map(_.toInt).toVector).get

  object Opcodes {
    val HALT = 99
    val ADD = 1
    val MULT = 2
  }

  @tailrec def exec(program: Vector[Int], ip: Int = 0): Vector[Int] = {
    import Opcodes._
    val opcode = program(ip)
    if (opcode == HALT) program
    else {
      val op1 = program(ip + 1)
      val op2 = program(ip + 2)
      val store = program(ip + 3)
      if (opcode == ADD) exec(program.updated(store, program(op1) + program(op2)), ip + 4)
      else if (opcode == MULT) exec(program.updated(store, program(op1) * program(op2)), ip + 4)
      else throw new IllegalStateException(s"? opcode $opcode")
    }
  }

  def execWithInput(noun: Int, verb: Int): Int = {
    exec(input.updated(1, noun).updated(2, verb))(0)
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
