package aoc2015

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day07 extends App {

  abstract sealed class Param {
    def eval(values: Map[String, Int]): Int
  }

  case class Ref(dest: String) extends Param {
    override def eval(values: Map[String, Int]): Int = values.getOrElse(dest, 0)
  }

  case class Const(value: Int) extends Param {
    override def eval(values: Map[String, Int]): Int = value
  }

  object Param {
    val variable: Regex = """([a-z]+)""".r
    val value: Regex = """([0-9]+)""".r

    def apply(s: String): Param = s match {
      case variable(x) => Ref(x)
      case value(x) => Const(x.toInt)
    }
  }

  abstract sealed class Connection(dest: String) {
    def getDest: String = dest

    def eval(values: Map[String, Int]): Int
  }

  case class Wire(ref: Param, dest: String) extends Connection(dest) {
    override def eval(values: Map[String, Int]): Int = ref.eval(values)
  }

  case class AndGate(a: Param, b: Param, dest: String) extends Connection(dest) {
    override def eval(values: Map[String, Int]): Int = a.eval(values) & b.eval(values)
  }

  case class OrGate(a: Param, b: Param, dest: String) extends Connection(dest) {
    override def eval(values: Map[String, Int]): Int = a.eval(values) | b.eval(values)
  }

  case class LShiftGate(a: Param, b: Param, dest: String) extends Connection(dest) {
    override def eval(values: Map[String, Int]): Int = a.eval(values) << b.eval(values)
  }

  case class RShiftGate(a: Param, b: Param, dest: String) extends Connection(dest) {
    override def eval(values: Map[String, Int]): Int = a.eval(values) >> b.eval(values)
  }

  case class NotGate(a: Param, dest: String) extends Connection(dest) {
    override def eval(values: Map[String, Int]): Int = 0xFFFF ^ a.eval(values)
  }

  object LineParser {
    val wire: Regex = """(\w+) -> (\w+)""".r
    val andGate: Regex = """(\w+) AND (\w+) -> (\w+)""".r
    val orGate: Regex = """(\w+) OR (\w+) -> (\w+)""".r
    val lShiftGate: Regex = """(\w+) LSHIFT (\w+) -> (\w+)""".r
    val rShiftGate: Regex = """(\w+) RSHIFT (\w+) -> (\w+)""".r
    val notGate: Regex = """NOT (\w+) -> (\w+)""".r

    def parseLine(line: String): Connection = line match {
      case wire(a, dest) => Wire(Param(a), dest)
      case andGate(a, b, dest) => AndGate(Param(a), Param(b), dest)
      case orGate(a, b, dest) => OrGate(Param(a), Param(b), dest)
      case lShiftGate(a, b, dest) => LShiftGate(Param(a), Param(b), dest)
      case rShiftGate(a, b, dest) => RShiftGate(Param(a), Param(b), dest)
      case notGate(a, dest) => NotGate(Param(a), dest)
    }
  }

  val input = Source.fromFile("inputs/2015/07.txt").getLines()
    .map(LineParser.parseLine)
    .toList

  def iterate(cxs: List[Connection], z: Map[String, Int]) = cxs.foldLeft(z) {
    case (map, cx) => map.updated(cx.getDest, cx.eval(map))
  }

  @tailrec def fixedPoint(cxs: List[Connection], z: Map[String, Int] = Map()): Map[String, Int] = {
    val q = iterate(cxs, z)
    if (q == z) q else fixedPoint(cxs, q)
  }

  val part1 = fixedPoint(input)("a")

  println(part1)

  val part2 = fixedPoint(Wire(Const(part1), "b") :: input.filter(cx => cx.getDest != "b"))("a")

  println(part2)
}
