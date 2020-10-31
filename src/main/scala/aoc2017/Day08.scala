package aoc2017

import scala.io.Source

object Day08 extends App {

  val re = """(\w+) (\w+) (-?\d+) if (\w+) (.*?) (-?\d+)""".r

  case class Instr(reg: String, amt: Int, cond: Cond, update: Int => Int) {
    def exec(map: Map[String, Int]): Map[String, Int] =
      if (cond satisfies map) map.updated(reg, update(map(reg)))
      else map
  }

  object Instr {
    def apply(reg: String, s: String, amt: Int, cond: Cond): Instr = s match {
      case "inc" => Instr(reg, amt, cond, _ + amt)
      case "dec" => Instr(reg, amt, cond, _ - amt)
    }
  }

  case class Cond(reg: String, cmp: Int, operator: (Int, Int) => Boolean) {
    def satisfies(map: Map[String, Int]): Boolean = operator(map(reg), cmp)
  }

  object Cond {
    def apply(reg: String, oper: String, cmp: Int): Cond = oper match {
      case ">" => Cond(reg, cmp, _ > _)
      case ">=" => Cond(reg, cmp, _ >= _)
      case "==" => Cond(reg, cmp, _ == _)
      case "!=" => Cond(reg, cmp, _ != _)
      case "<" => Cond(reg, cmp, _ < _)
      case "<=" => Cond(reg, cmp, _ <= _)
    }
  }

  val input = Source.fromFile("inputs/2017/08.txt").getLines().map {
    case re(a, b, c, d, e, f) => Instr(a, b, c.toInt, Cond(d, e, f.toInt))
  }.toList

  val memory = input.scanLeft(Map[String, Int]() withDefaultValue 0) {
    case (memory, instr) => instr exec memory
  }

  println(memory.last.values.max)
  println(memory.flatMap(_.values).max)
}
