package aoc2022

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source
import scala.util.Using

object Day11 extends App {

  case class Monkey(id: Int, items: Queue[Long], op: Long => Long, test: Long, ifTrue: Int, ifFalse: Int, inspectionCount: Long)

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
        case s"Starting items: $items" => Queue.from(items.split(",").map(_.trim).map(_.toLong))
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

  val monkeys: Map[Int, Monkey] = Using(Source.fromFile("inputs/2022/11.txt"))(_.getLines().mkString("\n").split("\n\n").map(_.split("\n").toList)).get
    .map(_.map(_.trim))
    .map(monkey)
    .map(m => m.id -> m)
    .toMap

  def throwTo(monkeys: Map[Int, Monkey], target: Int, item: Long): Map[Int, Monkey] = {
    monkeys.updated(target, monkeys(target).copy(items = monkeys(target).items :+ item))
  }

  @tailrec
  def turn(monkey: Monkey, monkeys: Map[Int, Monkey], divisor: Int): Map[Int, Monkey] = {
    if (monkey.items.isEmpty) monkeys.updated(monkey.id, monkey)
    else {
      val item = monkey.op(monkey.items.head) / divisor
      val target = if (item % monkey.test == 0) monkey.ifTrue else monkey.ifFalse
      val newMonkey = monkey.copy(items = monkey.items.tail, inspectionCount = monkey.inspectionCount + 1)
      turn(newMonkey, throwTo(monkeys, target, item % monkeys.values.map(_.test).product), divisor)
    }
  }

  val turns = monkeys.values.map(_.id).toList.sorted

  def round(divisor: Int)(monkeys: Map[Int, Monkey]): Map[Int, Monkey] = {
    turns.foldLeft(monkeys) {
      case (monkeys, id) => turn(monkeys(id), monkeys, divisor)
    }
  }

  def monkeyBusiness(rounds: Int, divisor: Int) = {
    LazyList.iterate(monkeys)(round(divisor)).drop(rounds).head
      .values
      .toList
      .map(_.inspectionCount)
      .sorted
      .reverse
      .take(2)
      .product
  }

  println(monkeyBusiness(20, 3))

  println(monkeyBusiness(10000, 1))
}
