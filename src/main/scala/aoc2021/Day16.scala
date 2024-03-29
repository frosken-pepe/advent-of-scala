package aoc2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day16 extends App {

  val input = Using(Source.fromFile("inputs/2021/16.txt"))(_.getLines()
    .toList).get

  @tailrec def padLeft(s: String): String = {
    if (s.length < 4) padLeft("0" + s)
    else s
  }

  def hex2bin(s: String): String = {
    s.map(ch => Integer.parseInt(s"$ch", 16))
      .map(Integer.toBinaryString)
      .map(padLeft)
      .mkString
  }

  val binary = hex2bin(input.head)

  sealed trait Packet {
    def sumVersion: Int

    def eval: Long
  }

  case class Literal(version: Int, value: Long) extends Packet {
    override def sumVersion: Int = version

    override def eval: Long = value
  }

  case class Operator(typeId: Int, version: Int, children: List[Packet]) extends Packet {
    override def sumVersion: Int = version + children.map(_.sumVersion).sum

    override def eval: Long = typeId match {
      case 0 => children.map(_.eval).sum
      case 1 => children.map(_.eval).product
      case 2 => children.map(_.eval).min
      case 3 => children.map(_.eval).max
      case 5 => if (children.head.eval > children.drop(1).head.eval) 1 else 0
      case 6 => if (children.head.eval < children.drop(1).head.eval) 1 else 0
      case 7 => if (children.head.eval == children.drop(1).head.eval) 1 else 0
    }
  }

  @tailrec def decodeMany(n: Int, binary: String, acc: List[Packet]): (List[Packet], String) = {
    if (n == 0) (acc.reverse, binary)
    else decodePacket(binary) match {
      case (packet, rest) => decodeMany(n - 1, rest, packet :: acc)
    }
  }

  @tailrec def decodeList(binary: String, acc: List[Packet]): List[Packet] = {
    if (binary.isEmpty) acc.reverse
    else decodePacket(binary) match {
      case (packet, rest) => decodeList(rest, packet :: acc)
    }
  }

  def decodeLiteral(binary: String): (Long, String) = {
    val grouped = binary.drop(6).sliding(5, 5).toList
    val startingBit = grouped.map(_.head)
    val toTake = startingBit.takeWhile(_ == '1').length + 1
    val bits = grouped.take(toTake).map(_.tail).mkString
    val literalValue = java.lang.Long.parseLong(bits, 2)
    (literalValue, binary.drop(6 + toTake * 5))
  }

  def decodePacket(binary: String): (Packet, String) = {
    val version = Integer.parseInt(binary.take(3), 2)
    val typeId = Integer.parseInt(binary.slice(3, 6), 2)
    if (typeId == 4) {
      decodeLiteral(binary) match {
        case (literalValue, rest) => (Literal(version, literalValue), rest)
      }
    } else {
      val lengthTypeId = binary.drop(6).head
      if (lengthTypeId == '0') {
        val lengthOfSubPackets = Integer.parseInt(binary.slice(7, 22), 2)
        val slice = binary.slice(22, lengthOfSubPackets + 22)
        (Operator(typeId, version, decodeList(slice, Nil)), binary.drop(22 + lengthOfSubPackets))
      } else {
        val numberOfSubPackets = Integer.parseInt(binary.slice(7, 18), 2)
        val rest = binary.drop(18)
        decodeMany(numberOfSubPackets, rest, Nil) match {
          case (value, rest) => (Operator(typeId, version, value), rest)
        }
      }
    }
  }

  val (packet, _) = decodePacket(binary)

  println(packet.sumVersion)

  println(packet.eval)
}
