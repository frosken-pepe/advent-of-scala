package aoc2020

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App {

  val emptyMask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

  val program = Source.fromFile("inputs/2020/14.txt").getLines().toList

  case class State(mask: String, memory: Map[Long, Long]) {

    @tailrec private def padLeft(s: String, len: Int, ch: Char): String = {
      if (s.length < len) padLeft(ch + s, len, ch)
      else s
    }

    def maskP1(value: Long): Long = {
      BigInt(
        padLeft(value.toBinaryString, 36, '0').zip(mask).map {
          case (_, '1') => '1'
          case (_, '0') => '0'
          case (v, 'X') => v
        }.mkString, 2).longValue
    }

    def maskP2(addr: Long): Set[Long] = {
      val masked = padLeft(addr.toBinaryString, 36, '0').zip(mask).map {
        case (_, '1') => '1'
        case (v, '0') => v
        case (_, 'X') => 'X'
      }.mkString.toList
      floatingBit(masked, List(Nil)).map(f => BigInt.apply(f.mkString, 2)).map(_.longValue).toSet
    }

    @tailrec private def floatingBit(mask: List[Char], acc: List[List[Char]]): List[List[Char]] = {
      if (mask.isEmpty) acc.map(_.reverse)
      else floatingBit(mask.tail,
        mask.head match {
          case '0' => acc.map(l => '0' :: l)
          case '1' => acc.map(l => '1' :: l)
          case 'X' => acc.flatMap(l => List('0' :: l, '1' :: l))
        })
    }

    def sumValues: Long = memory.values.sum
  }

  val mask = """mask = (.*)""".r
  val mem = """mem\[(\d+)] = (\d+)""".r

  println(
    program.foldLeft(State(emptyMask, Map())) {
      case (state, line) => line match {
        case mask(m) => state.copy(mask = m)
        case mem(loc, value) => state.copy(memory = state.memory.updated(loc.toLong, state.maskP1(value.toLong)))
      }
    }.sumValues
  )

  println(
    program.foldLeft(State(emptyMask, Map())) {
      case (state, line) => line match {
        case mask(m) => state.copy(mask = m)
        case mem(loc, value) => state.copy(memory = state.maskP2(loc.toLong).foldLeft(state.memory) {
          case (mem, addr) => mem.updated(addr, value.toLong)
        })
      }
    }.sumValues
  )
}
