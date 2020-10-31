package aoc2017

import scala.io.Source

object Day22 extends App {

  sealed trait NodeState {
    def next(part: Int): NodeState
  }

  case object Clean extends NodeState {
    override def next(part: Int): NodeState = if (part == 1) Infected else Weakened
  }

  case object Weakened extends NodeState {
    override def next(part: Int): NodeState = Infected
  }

  case object Infected extends NodeState {
    override def next(part: Int): NodeState = if (part == 1) Clean else Flagged
  }

  case object Flagged extends NodeState {
    override def next(part: Int): NodeState = Clean
  }

  val source = Source.fromFile("inputs/2017/22.txt").getLines().toList

  val nodes: Map[(Int, Int), NodeState] = source
    .zipWithIndex
    .flatMap {
      case (row, y) => row.zipWithIndex.flatMap {
        case ('#', x) => Some((x, y) -> Infected)
        case _ => None
      }
    }
    .toMap
    .withDefaultValue(Clean)

  val startPos = ((source.length - 1) / 2, (source.head.length - 1) / 2)

  case class State(carrierPos: (Int, Int), carrierDir: Int, nodes: Map[(Int, Int), NodeState], newInfected: Int) {

    def burst(part: Int): State = copy(
      carrierDir = newCarrierDir,
      nodes = nodes.updated(carrierPos, nodes(carrierPos).next(part)),
      newInfected = newInfected + (if (nodes(carrierPos).next(part) == Infected) 1 else 0)
    ).walk

    def newCarrierDir: Int = nodes(carrierPos) match {
      case Clean => (4 + carrierDir - 1) % 4
      case Weakened => carrierDir
      case Infected => (carrierDir + 1) % 4
      case Flagged => (carrierDir + 2) % 4
    }

    def walk: State = carrierDir match {
      case 0 => copy(carrierPos = (carrierPos._1, carrierPos._2 - 1))
      case 1 => copy(carrierPos = (carrierPos._1 + 1, carrierPos._2))
      case 2 => copy(carrierPos = (carrierPos._1, carrierPos._2 + 1))
      case 3 => copy(carrierPos = (carrierPos._1 - 1, carrierPos._2))
    }
  }

  val initialState = State(startPos, 0, nodes, 0)

  println(LazyList.iterate(initialState)(_.burst(1)).map(_.newInfected).drop(10000).head)
  println(LazyList.iterate(initialState)(_.burst(2)).map(_.newInfected).drop(10000000).head)
}
