package aoc2016

import scala.io.Source

object Day04 extends App {

  val re = """((?:\w+-)*\w+)-(\d+)\[(\w+)]""".r

  case class Room(encName: String, sectorId: Int, checksum: String)

  val input = Source.fromFile("inputs/2016/04.txt")
    .getLines()
    .map { case re(encName, sectorId, checksum) => Room(encName, sectorId.toInt, checksum) }
    .toList

  def checksum(encName: String): String = {
    encName.filter(_.isLower).foldLeft(Map[Char, Int]()) {
      case (map, ch) => map.updated(ch, map.getOrElse(ch, 0) + 1)
    }.toList.sortBy(-_._1).sortBy(_._2).reverse.map(_._1).take(5).mkString
  }

  def isReal(room: Room) = room.checksum == checksum(room.encName)

  println(input.filter(isReal).map(_.sectorId).sum)

  def decrypt(room: Room): String = {
    room.encName.map {
      case '-' => " "
      case ch => ('a' + (ch - 'a' + room.sectorId) % 26).toChar
    }.mkString
  }

  println(
    input.filter(isReal)
      .filter(room => decrypt(room) == "northpole object storage")
      .map(_.sectorId)
      .head)
}
