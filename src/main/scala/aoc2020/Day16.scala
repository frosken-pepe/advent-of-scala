package aoc2020

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day16 extends App {

  val (specs, yourTicket, nearbyTickets) = {
    val lines = Using(Source.fromFile("inputs/2020/16.txt"))(_.getLines().toList).get
    val idxNearbyTickets = lines.zipWithIndex.find(_._1 == "nearby tickets:").head._2
    val nearbyTickets = lines.drop(idxNearbyTickets + 1)
      .map(_.split(",").map(_.toInt).toList)
    val specs = lines.takeWhile(_ != "").map(_.split(": ").toList).map {
      case k :: v :: Nil => k -> v.split(" or ").toList.map(_.split("-").toList match {
        case from :: to :: Nil => from.toInt to to.toInt
      })
    }.toMap
    val yourTicket = lines.dropWhile(_ != "your ticket:").drop(1).head.split(",").map(_.toInt).toList
    (specs, yourTicket, nearbyTickets)
  }

  def invalidValues(ticket: List[Int]): List[Int] = {
    val ranges = specs.values.flatten
    ticket.filter(t => !ranges.exists(_.contains(t)))
  }

  println(nearbyTickets.flatMap(invalidValues).sum)

  def isValid(ticket: List[Int]) = invalidValues(ticket).isEmpty

  val validNearbyTickets = nearbyTickets.filter(isValid)

  val numFields = validNearbyTickets.head.length

  def deduceField(fieldNo: Int, discovered: Set[String]): Option[String] = {
    val fieldValues = validNearbyTickets.map(_ (fieldNo))
    val temp = specs.filter {
      case (k, ranges) if !discovered.contains(k) => fieldValues.forall(v => ranges.exists(_.contains(v)))
      case _ => false
    }.keys.toSet
    if (temp.size == 1) Some(temp.head) else None
  }

  @tailrec def deduceFields(discovered: Map[Int, String]): Map[Int, String] = {
    if (discovered.size == numFields) discovered
    else {
      val (idx, fieldName) = (for {
        unknown <- 0 until numFields if !discovered.contains(unknown)
        deduced <- deduceField(unknown, discovered.values.toSet)
      } yield (unknown, deduced)).head
      deduceFields(discovered.updated(idx, fieldName))
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
