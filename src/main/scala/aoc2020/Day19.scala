package aoc2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day19 extends App {

  sealed abstract class Rule(val id: Int)

  case class SingleCharRule(_id: Int, ch: Char) extends Rule(_id)

  case class Choice(_id: Int, deps: List[List[Int]]) extends Rule(_id)

  val (rules, messages) = {

    val lines = Using(Source.fromFile("inputs/2020/19.txt"))(_.getLines().toList).get

    val singleCharRule = """(\d+): "([a-z])"""".r
    val dependentRule = """(\d+): (.*)""".r

    def parseIds(s: String): List[List[Int]] = {
      s.split('|').map(_.trim).map(_.split(" ").map(_.toInt).toList).toList
    }

    val rules: Map[Int, Rule] = lines.takeWhile(_ != "").map {
      case singleCharRule(id, ch) => SingleCharRule(id.toInt, ch.head)
      case dependentRule(id, rest) => Choice(id.toInt, parseIds(rest))
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
      case Choice(_, deps) => for {
        group: List[Int] <- deps.toSet
        concat <- prod(group.map(g => possibleMessages(rules, g, acc)))
      } yield concat
    }
  }

  println((possibleMessages(rules, 0) intersect messages).size)

  val messageSubstrings = for {
    message <- messages
    startIdx <- 0 until message.length
    endIdx <- startIdx to message.length
  } yield message.substring(startIdx, endIdx)

  val rule42 = possibleMessages(rules, 42).filter(_.length <= maxMessageLen)

  val rule31 = possibleMessages(rules, 31).filter(_.length <= maxMessageLen)

  @tailrec def genRule8(prev: Set[String], seen: Set[String] = Set()): Set[String] = {
    val newRules: Set[String] = (for {
      r42 <- rule42
      a <- prev
      if a.length + r42.length <= maxMessageLen
      cd = a + r42
      if !seen.contains(cd)
      if messageSubstrings.contains(cd)
    } yield cd)
    if (newRules.isEmpty) seen else genRule8(newRules, seen ++ newRules)
  }

  @tailrec def genRule11(prev: Set[String], seen: Set[String] = Set()): Set[String] = {
    val newRules: Set[String] = (for {
      r42 <- rule42
      a <- prev
      if messageSubstrings.contains(r42 + a)
      r31 <- rule31
      if r42.length + a.length + r31.length <= maxMessageLen
      cd = r42 + a + r31
      if !seen.contains(cd)
      if messageSubstrings.contains(cd)
    } yield cd)
    if (newRules.isEmpty) seen else genRule11(newRules, seen ++ newRules)
  }

  val rule8 = genRule8(Set(""))

  val rule11 = genRule11(Set(""))

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

  // 200, 407
}
