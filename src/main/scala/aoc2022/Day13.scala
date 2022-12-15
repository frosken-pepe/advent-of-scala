package aoc2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day13 extends App {

  val input = Using(Source.fromFile("inputs/2022/13.txt"))(_.getLines().toList).get.sliding(3, 3).toList

  sealed trait Packet

  case class IntegerPacket(value: Int) extends Packet

  case class ListPacket(values: List[Packet]) extends Packet

  def integer(s: String): Option[(IntegerPacket, String)] = {
    val digits = s.takeWhile(_.isDigit)
    if (digits.isEmpty) None
    else Some(IntegerPacket(digits.toInt), s.drop(digits.length))
  }

  def literal(s: String, lit: String): Option[(String, String)] = {
    if (s.startsWith(lit)) Some(lit, s.drop(lit.length))
    else None
  }

  @tailrec
  def commaSeparatedPackets(s: String, acc: List[Packet]): (List[Packet], String) = {
    val optPacket = integerOrList(s)
    if (optPacket.isDefined) commaSeparatedPackets(optPacket.get._2.dropWhile(_ == ','), optPacket.get._1 :: acc)
    else (acc.reverse, s)
  }

  def list(s: String): Option[(ListPacket, String)] = {
    for {
      (_, r0) <- literal(s, "[")
      (packets, r1) = commaSeparatedPackets(r0, Nil)
      (_, rest) <- literal(r1, "]")
    } yield (ListPacket(packets), rest)
  }

  def integerOrList(s: String): Option[(Packet, String)] = {
    integer(s).orElse(list(s))
  }

  def parse(s: String): List[Packet] = {
    list(s).get._1.values
  }

  def isRightOrder(a: List[Packet], b: List[Packet]): Option[Boolean] = (a, b) match {
    case ((ha: IntegerPacket) :: ta, (hb: IntegerPacket) :: tb) =>
      if (ha.value != hb.value) Some(ha.value < hb.value)
      else isRightOrder(ta, tb)
    case ((ha: ListPacket) :: ta, (hb: ListPacket) :: tb) => isRightOrder(ha.values, hb.values).orElse(isRightOrder(ta, tb))
    case (Nil, _ :: _) => Some(true)
    case (_ :: _, Nil) => Some(false)
    case ((ha: IntegerPacket) :: ta, (hb: ListPacket) :: tb) => isRightOrder(ListPacket(List(ha)) :: ta, hb :: tb).orElse(isRightOrder(ta, tb))
    case ((ha: ListPacket) :: ta, (hb: IntegerPacket) :: tb) => isRightOrder(ha :: ta, ListPacket(List(hb)) :: tb).orElse(isRightOrder(ta, tb))
    case (Nil, Nil) => None
  }

  println(
    input.zipWithIndex.map {
      case (pair, idx) if isRightOrder(parse(pair.head), parse(pair.tail.head)).get => idx + 1
      case _ => 0
    }.sum)

  def lt(a: List[Packet], b: List[Packet]): Boolean = isRightOrder(a, b).get

  val dividers: List[List[Packet]] = List(
    List(ListPacket(List(IntegerPacket(2)))),
    List(ListPacket(List(IntegerPacket(6)))),
  )

  val packets: List[List[Packet]] = dividers ++ input.flatMap {
    case a :: b :: _ => List(parse(a), parse(b))
  }

  val sorted = packets.sortWith(lt)

  println((sorted.indexOf(dividers.tail.head) + 1) * (sorted.indexOf(dividers.head) + 1))
}
