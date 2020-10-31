package aoc2017

import scala.io.Source

object Day25 extends App {

  case class Transition(write: Int, move: Int, nextState: String)

  case class Transitions(zero: Transition, one: Transition) {
    def get(i: Int): Transition = i match {
      case 0 => zero
      case 1 => one
    }
  }

  object Input {

    private val input = Source.fromFile("inputs/2017/25.txt").getLines().toList

    private val reStartState = """Begin in state (\w+)\.""".r
    val startState: String = input.head match {
      case reStartState(s) => s
    }

    private val reChecksum = """Perform a diagnostic checksum after (\d+) steps.""".r
    val checksum: Int = input.tail.head match {
      case reChecksum(ch) => ch.toInt
    }

    private val stateDescriptions = input.drop(3).sliding(10, 10)
      .map(_.take(9)).map(_.mkString(System.lineSeparator())).toList
    private val reState =
      """|In state (\w+):
         |  If the current value is 0:
         |    - Write the value (0|1).
         |    - Move one slot to the (left|right).
         |    - Continue with state (\w+).
         |  If the current value is 1:
         |    - Write the value (0|1).
         |    - Move one slot to the (left|right).
         |    - Continue with state (\w+).""".stripMargin.r

    private def leftRight(s: String): Int = s match {
      case "left" => -1
      case "right" => 1
    }

    val trans: Map[String, Transitions] = stateDescriptions.map {
      case reState(inState, w0, m0, s0, w1, m1, s1) => (inState, Transitions(
        Transition(w0.toInt, leftRight(m0), s0),
        Transition(w1.toInt, leftRight(m1), s1)
      ))
    }.toMap
  }

  import Input._

  case class Machine(state: String, headPos: Int, tape: Map[Int, Int])

  val tape = LazyList.iterate(Machine(startState, 0, Map())) { machine =>
    trans(machine.state).get(machine.tape.getOrElse(machine.headPos, 0)) match {
      case Transition(write, move, next) => machine.copy(
        tape = machine.tape.updated(machine.headPos, write),
        headPos = machine.headPos + move,
        state = next
      )
    }
  }.drop(checksum).head.tape

  println(tape.values.count(i => i == 1))
}
