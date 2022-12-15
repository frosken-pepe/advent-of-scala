package aoc2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day13 extends App {

  sealed trait Packet

  case class IntPacket(value: Int) extends Packet

  case class ListPacket(values: List[Packet]) extends Packet

  def integer(s: String): Option[(IntPacket, String)] = {
    val digits = s.takeWhile(_.isDigit)
    if (digits.isEmpty) None
    else Some(IntPacket(digits.toInt), s.drop(digits.length))
  }

  def literal(s: String, lit: String): Option[String] = {
    if (s.startsWith(lit)) Some(s.drop(lit.length))
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
      r0 <- literal(s, "[")
      (packets, r1) = commaSeparatedPackets(r0, Nil)
      rest <- literal(r1, "]")
    } yield (ListPacket(packets), rest)
  }

  def integerOrList(s: String): Option[(Packet, String)] = integer(s).orElse(list(s))

  def parse(s: String): List[Packet] = list(s).get._1.values

  val input: List[(List[Packet], List[Packet])] = Using(Source.fromFile("inputs/2022/13.txt"))(_.getLines().toList)
    .get.sliding(3, 3).toList.map { case a :: b :: _ => (parse(a), parse(b)) }

  def isRightOrder(a: List[Packet], b: List[Packet]): Option[Boolean] = (a, b) match {
    case (Nil, Nil) => None
    case (Nil, _) => Some(true)
    case (_, Nil) => Some(false)
    case (IntPacket(ha) :: ta, IntPacket(hb) :: tb) => if (ha != hb) Some(ha < hb) else isRightOrder(ta, tb)
    case (ListPacket(ha) :: ta, ListPacket(hb) :: tb) => isRightOrder(ha, hb).orElse(isRightOrder(ta, tb))
    case ((ha: IntPacket) :: ta, (hb: ListPacket) :: tb) => isRightOrder(ListPacket(List(ha)) :: ta, hb :: tb).orElse(isRightOrder(ta, tb))
    case ((ha: ListPacket) :: ta, (hb: IntPacket) :: tb) => isRightOrder(ha :: ta, ListPacket(List(hb)) :: tb).orElse(isRightOrder(ta, tb))
  }

  def lt(a: List[Packet], b: List[Packet]): Boolean = isRightOrder(a, b).get

  println(input.zipWithIndex.filter(z => lt(z._1._1, z._1._2)).map(_._2 + 1).sum)

  val dividers: List[List[Packet]] = List("[[2]]", "[[6]]").map(parse)

  val packets: List[List[Packet]] = dividers ++ input.flatMap(a => List(a._1, a._2))

  val sorted = packets.sortWith(lt)

  println((sorted.indexOf(dividers.tail.head) + 1) * (sorted.indexOf(dividers.head) + 1))
}
