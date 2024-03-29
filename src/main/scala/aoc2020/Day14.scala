package aoc2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day14 extends App {

  val emptyMask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"

  val program = Using(Source.fromFile("inputs/2020/14.txt"))(_.getLines().toList).get

  case class State(mask: String, memory: Map[Long, Long]) {

    private def padLeft(s: String, len: Int, ch: Char): String =
      (0 until len - s.length).map(_ => ch).mkString + s

    def maskP1(value: Long): Long = {
      BigInt(
        padLeft(value.toBinaryString, 36, '0').zip(mask).map {
          case (_, '1') => '1'
          case (_, '0') => '0'
          case (v, 'X') => v
        }.mkString, 2).longValue
    }

    def maskP2(addr: Long): Set[Long] =
      floatingBit(padLeft(addr.toBinaryString, 36, '0').zip(mask).map {
        case (_, '1') => '1'
        case (v, '0') => v
        case (_, 'X') => 'X'
      }).map(BigInt(_, 2).longValue).toSet

    @tailrec private def floatingBit(mask: Seq[Char], acc: List[List[Char]] = List(Nil)): List[String] = {
      if (mask.isEmpty) acc.map(_.reverse).map(_.mkString)
      else floatingBit(mask.tail,
        mask.head match {
          case 'X' => acc.flatMap(l => List('0' :: l, '1' :: l))
          case x => acc.map(x :: _)
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
