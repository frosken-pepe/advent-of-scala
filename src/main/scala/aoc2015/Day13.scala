package aoc2015

import scala.io.Source
import scala.util.{Try, Using}

object Day13 extends App {

  case class Seating(person: String, neighbor: String, happiness: Int)

  object Seating {
    private val seating = """(\w+) would (lose|gain) (\d+) happiness units by sitting next to (\w+).""".r

    def apply(s: String): Seating = s match {
      case seating(person, "lose", happiness, neighbor) => Seating(person, neighbor, -happiness.toInt)
      case seating(person, "gain", happiness, neighbor) => Seating(person, neighbor, happiness.toInt)
    }
  }

  val input = Using(Source.fromFile("inputs/2015/13.txt"))(_.getLines()
    .map(Seating.apply)
    .toList).get

  def maximizeHappiness(seats: List[Seating]) = {

    val guests = seats.flatMap(seating => List(seating.person, seating.neighbor)).distinct

    val pairScores: Map[(String, String), Int] = guests.flatMap(a => guests.map(b => (a, b)))
      .filter(pair => pair._1 != pair._2)
      .map {
        case p@(a, b) =>
          val x = seats.find(it => it.person == a && it.neighbor == b).map(_.happiness)
          val y = seats.find(it => it.person == b && it.neighbor == a).map(_.happiness)
          p -> (x.getOrElse(0) + y.getOrElse(0))
      }
      .toMap
      .withDefaultValue(0)

    def happiness(order: List[String]): Int = order.zip(order.drop(1) :+ order.head).map(pairScores).sum

    // rotations of any seating are equivalent
    val permutations = guests.tail.permutations.map(guests.head :: _).toList

    happiness(permutations.maxBy(happiness))
  }

  println(maximizeHappiness(input))
  println(maximizeHappiness(new Seating("You", "You", 0) :: input))
}
