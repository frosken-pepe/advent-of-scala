package aoc2022

import aoc2021.Day13.draw

import scala.io.Source
import scala.util.Using

object Day10 extends App {

  sealed trait Instruction

  case class Addx(x: Int) extends Instruction

  case object Noop extends Instruction

  val input = Using(Source.fromFile("inputs/2022/10.txt"))(_.getLines().map {
    case s"addx $x" => Addx(x.toInt)
    case "noop" => Noop
  }.toList).get

  case class State(time: Int, x: Int)

  val states =
    input.scanLeft(State(x = 1, time = 0)) {
      case (state, Noop) => state.copy(time = state.time + 1)
      case (state, Addx(xx)) => state.copy(x = state.x + xx, time = state.time + 2)
    }

  def valueOfXAt(time: Int): Int = {
    states.foldLeft(0) {
      case (_, update) if update.time + 1 <= time => update.x
      case (x, _) => x
    }
  }

  def signalStrength(time: Int) = {
    time * valueOfXAt(time)
  }

  println(List(20, 60, 100, 140, 180, 220).map(signalStrength).sum)

  draw(
    for {
      _ <- Set(())
      cycle <- 0 until 240
      row = cycle / 40
      col = cycle % 40
      x = valueOfXAt(cycle + 1)
      lit = Set(x - 1, x, x + 1)
      if lit.contains(col)
    } yield (col, row))
}
