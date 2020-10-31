package aoc2015

import scala.io.Source

object Day16 extends App {

  val analysis = Map(
    "children" -> 3,
    "cats" -> 7,
    "samoyeds" -> 2,
    "pomeranians" -> 3,
    "akitas" -> 0,
    "vizslas" -> 0,
    "goldfish" -> 5,
    "trees" -> 3,
    "cars" -> 2,
    "perfumes" -> 1,
  )

  case class Sue(id: Int, props: Map[String, Int])

  object Sue {
    private val sue = """Sue (\d+):""".r.unanchored

    def apply(s: String): Sue = s match {
      case s@sue(id) =>
        val propsString = s.split(": ", 2)(1)
        val propsPairs = propsString.split(", ").toList
        val props = propsPairs.map(_.split(": ")).map(arr => arr(0) -> arr(1).toInt).toMap
        new Sue(id.toInt, props)
    }
  }

  val input = Source.fromFile("inputs/2015/16.txt").getLines().map(Sue.apply).toList

  def isSueP1(sue: Sue): Boolean = sue.props.forall {
    case (prop, quantity) => quantity == analysis(prop)
  }

  input.filter(isSueP1).map(_.id).foreach(println)

  def isSueP2(sue: Sue): Boolean = sue.props.forall {
    case (prop, quantity) if prop == "cats" || prop == "trees" => quantity > analysis(prop)
    case (prop, quantity) if prop == "pomeranians" || prop == "goldfish" => quantity < analysis(prop)
    case (prop, quantity) => quantity == analysis(prop)
  }

  input.filter(isSueP2).map(_.id).foreach(println)
}
