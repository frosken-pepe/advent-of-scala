package aoc2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day11 extends App {

  case class Monkey(id: Int, items: List[Long], op: Long => Long, test: Long, ifTrue: Int, ifFalse: Int, inspectionCount: Long)

  def parseOp(op: String): Long => Long = op match {
    case "old * old" => old => old * old
    case s"old * $a" => _ * a.toInt
    case s"old + $a" => _ + a.toInt
  }

  def monkey(ss: List[String]): Monkey = {
    Monkey(
      id = ss.head match {
        case s"Monkey $id:" => id.toInt
      },
      items = ss(1) match {
        case s"Starting items: $items" => items.split(",").map(_.trim).map(_.toLong).toList
      },
      op = ss(2) match {
        case s"Operation: new = $op" => parseOp(op)
      },
      test = ss(3) match {
        case s"Test: divisible by $a" => a.toInt
      },
      ifTrue = ss(4) match {
        case s"If true: throw to monkey $a" => a.toInt
      },
      ifFalse = ss(5) match {
        case s"If false: throw to monkey $a" => a.toInt
      },
      inspectionCount = 0
    )
  }

  val monkeys = Using(Source.fromFile("inputs/2022/11.txt"))(_.getLines().mkString("\n").split("\n\n").map(_.split("\n").toList)).get
    .map(strings => strings.map(_.trim))
    .map(monkey)
    .toList

  def throwTo(monkeys: List[Monkey], target: Int, worry: Long): List[Monkey] = {
    monkeys.map(m => if (m.id == target) m.copy(items = m.items ++ List(worry)) else m)
  }

  @tailrec
  def turn(monkey: Monkey, monkeys: List[Monkey], divisor: Int): List[Monkey] = {
    if (monkey.items.isEmpty) monkeys.map(m => if (m.id == monkey.id) monkey else m)
    else {
      val item = monkey.op(monkey.items.head) / divisor
      val target = if (item % monkey.test == 0) monkey.ifTrue else monkey.ifFalse
      val newMonkey = monkey.copy(items = monkey.items.tail, inspectionCount = monkey.inspectionCount + 1)
      turn(newMonkey, throwTo(monkeys, target, item % monkeys.map(_.test).product), divisor)
    }
  }

  def round(divisor: Int)(monkeys: List[Monkey]): List[Monkey] = {
    val turns = monkeys.map(_.id)
    turns.foldLeft(monkeys) {
      case (monkeys, id) => turn(monkeys.find(_.id == id).get, monkeys, divisor)
    }
  }

  def monkeyBusiness(rounds: Int, divisor: Int) = {
    LazyList.iterate(monkeys)(round(divisor)).drop(rounds).head
      .map(_.inspectionCount)
      .sorted
      .reverse
      .take(2)
      .product
  }

  println(monkeyBusiness(20, 3))

  println(monkeyBusiness(10000, 1))
}
