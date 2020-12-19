package aoc2020

import scala.annotation.tailrec
import scala.io.Source

object Day19 extends App {

  sealed abstract class Rule(val id: Int)

  case class SingleCharRule(_id: Int, ch: Char) extends Rule(_id)

  case class DependentRule(_id: Int, deps: List[List[Int]]) extends Rule(_id)

  val (rules, messages) = {

    val lines = Source.fromFile("inputs/2020/19.txt").getLines().toList

    val singleCharRule = """(\d+): "([a-z])"""".r
    val dependentRule = """(\d+): (.*)""".r

    def parseIds(s: String): List[List[Int]] = {
      s.split('|').map(_.trim).map(_.split(" ").map(_.toInt).toList).toList
    }

    val rules: Map[Int, Rule] = lines.takeWhile(_ != "").map {
      case singleCharRule(id, ch) => SingleCharRule(id.toInt, ch.head)
      case dependentRule(id, rest) => DependentRule(id.toInt, parseIds(rest))
    }.map(rule => rule.id -> rule).toMap

    val messages = lines.drop(rules.size + 1).toSet
    (rules, messages)
  }

  val maxMessageLen = messages.map(_.length).max

  @tailrec def prod(todo: List[Set[String]], acc: Set[String] = Set("")): Set[String] = {
    if (todo.isEmpty) acc
    else {
      val hd = todo.head
      prod(todo.tail, hd.flatMap(h => acc.map(a => a + h)))
    }
  }

  def possibleMessages(rules: Map[Int, Rule], ruleId: Int, acc: Set[String] = Set("")): Set[String] = {
    rules(ruleId) match {
      case SingleCharRule(_, ch) => acc.map(_ + ch)
      case DependentRule(_, deps) => for {
        group: List[Int] <- deps.toSet
        concat <- prod(group.map(g => possibleMessages(rules, g, acc)))
      } yield concat
    }
  }

  println((possibleMessages(rules, 0) intersect messages).size)

  val maxReps = 10000

  val rule42 = possibleMessages(rules, 42).filter(_.length <= maxMessageLen).filter(r => messages.exists(_.contains(r)))
  val rule31 = possibleMessages(rules, 31).filter(_.length <= maxMessageLen).filter(r => messages.exists(_.contains(r)))

  def genRule8(acc: Set[String] = Set()): Set[String] = {
    val generated = rule42.flatMap(r42 => (if (acc.isEmpty) Set("") else acc).flatMap { a =>
      if (a.length + r42.length <= maxMessageLen) {
        val cd = a + r42
        if (messages.exists(_.contains(cd))) Some(cd) else None
      } else None
    }) -- acc
    println(s"generated ${generated.size} new rule 8")
    if (generated.isEmpty) acc else acc ++ genRule8(generated)
  }

  def genRule11(acc: Set[String] = Set()): Set[String] = {
    val generated =
      rule31.flatMap(r31 => rule42.flatMap(r42 => (if (acc.isEmpty) Set("") else acc).flatMap { a =>
        if (r42.length + a.length + r31.length <= maxMessageLen) {
          val cd = r42 + a + r31
          if (messages.exists(_.contains(cd))) Some(cd) else None
        } else None
      })) -- acc
    println(s"generated ${generated.size} new rule 11")
    if (generated.isEmpty) acc else acc ++ genRule11( generated)
  }

  val rule8 = genRule8()
  println("calculated rules 8")

  val rule11 = genRule11()
  println("calculated rules 11")

  def rule0(s: String): Boolean = {
    (for {
      r8 <- rule8
      if s.startsWith(r8)
      r11 <- rule11
      if s.endsWith(r11)
      if (r8.length + r11.length) == s.length
    } yield ()).nonEmpty
  }

  println(messages.count(rule0))
}
