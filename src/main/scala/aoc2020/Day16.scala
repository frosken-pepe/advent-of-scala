package aoc2020

import scala.annotation.tailrec
import scala.io.Source

object Day16 extends App {

  val (specs, yourTicket, nearbyTickets) = {
    val lines = Source.fromFile("inputs/2020/16.txt").getLines().toList
    val idxNearbyTickets = lines.zipWithIndex.find(_._1 == "nearby tickets:").head._2
    val nearbyTickets = lines.drop(idxNearbyTickets + 1)
      .map(_.split(",").map(_.toInt).toList)
    val specs = lines.takeWhile(_ != "").map(_.split(": ").toList).map {
      case k :: v :: Nil => k -> v.split(" or ").toList.map(_.split("-").toList match {
        case from :: to :: Nil => (from.toInt, to.toInt)
      })
    }.toMap
    val yourTicket = lines.dropWhile(_ != "your ticket:").drop(1).head.split(",").map(_.toInt).toList
    (specs, yourTicket, nearbyTickets)
  }

  def invalidValues(ticket: List[Int]): List[Int] = {
    val ranges = specs.values.flatten
    ticket.filter(t => !ranges.exists(r => (r._1 to r._2).contains(t)))
  }

  println(nearbyTickets.flatMap(invalidValues).sum)

  def isValid(ticket: List[Int]) = invalidValues(ticket).isEmpty

  val validNearbyTickets = nearbyTickets.filter(isValid)

  val numFields = validNearbyTickets.head.length

  def deduceField(fieldNo: Int, discovered: Set[String]): Option[String] = {
    val fieldValues = validNearbyTickets.map(_ (fieldNo))
    val temp = specs.filter {
      case (k, ranges) if !discovered.contains(k) =>
        fieldValues.forall(v => ranges.exists(r => (r._1 to r._2).contains(v)))
      case _ => false
    }.keys.toSet
    if (temp.size == 1) Some(temp.head) else None
  }

  @tailrec def deduceFields(discovered: Map[Int, String]): Map[Int, String] = {
    if (discovered.size == numFields) discovered
    else {
      val entry = (for {
        i <- 0 until numFields
        deduced <- deduceField(i, discovered.values.toSet)
      } yield (i, deduced)).head
      deduceFields(discovered.updated(entry._1, entry._2))
    }
  }

  val mapping = deduceFields(Map())

  println(
    mapping.map {
      case (idx, fieldName) if fieldName.startsWith("departure") => yourTicket(idx).toLong
      case _ => 1L
    }.product
  )
}
