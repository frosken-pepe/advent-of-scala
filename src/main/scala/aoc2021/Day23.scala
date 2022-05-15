package aoc2021

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day23 extends App {

  sealed trait Amphipod {
    val targetSideRoom: Int
    val stepEnergy: Int
  }

  case object A extends Amphipod {
    override val targetSideRoom: Int = 3
    override val stepEnergy: Int = 1
  }
  case object B extends Amphipod {
    override val targetSideRoom: Int = 5
    override val stepEnergy: Int = 10
  }
  case object C extends Amphipod {
    override val targetSideRoom: Int = 7
    override val stepEnergy: Int = 100
  }
  case object D extends Amphipod {
    override val targetSideRoom: Int = 9
    override val stepEnergy: Int = 1000
  }

  object Amphipod {
    def fromChar(ch: Char): Amphipod = ch match {
      case 'A' => A
      case 'B' => B
      case 'C' => C
      case 'D' => D
    }
  }

  val input =
    Using(Source.fromFile("inputs/2021/23.txt"))(_.getLines().toList).get

  val open = input.zipWithIndex.flatMap { case (row, y) =>
    row.zipWithIndex.flatMap {
      case (ch, x) if ".ABCD".contains(ch) => Some(x, y)
      case _                               => None
    }
  }.toSet

  case class Point(x: Int, y: Int)

  val startPos = input.zipWithIndex.flatMap { case (row, y) =>
    row.zipWithIndex.flatMap {
      case (ch, x) if "ABCD".contains(ch) =>
        Some((Amphipod.fromChar(ch), Point(x, y)))
      case _ => None
    }
  }.toSet

  case class Move(a: Amphipod, from: Point, to: Point) {
    def expend: Int =
      (math.abs(from.x - to.x) + math.abs(from.y - to.y)) * a.stepEnergy
  }

  case class State(
      hall: Map[Int, Amphipod], // x-pos -> amphipod
      rooms: Map[Int, List[Amphipod]], // x-pos -> pods in room
      sideRoomSize: Int
  ) {

    assert(rooms.values.forall(_.size <= sideRoomSize))

    def isFinal: Boolean = hall.isEmpty && rooms.forall { case (x, as) =>
      as.forall(a => a.targetSideRoom == x)
    }

    def isClearPathHallway(from: Int, to: Int): Boolean = {
      val blocked = hall.keys.toSet
      if (blocked(to)) false
      else if (to > from) !(from + 1 until to).exists(blocked)
      else !(to + 1 until from).exists(blocked)
    }

    def validMoves: Set[Move] = {
      val hallIntoSideRoom: Set[Move] = for {
        (fromX, a) <- hall.toSet
        toX = a.targetSideRoom
        if toX != fromX
        if rooms(toX).forall(_ == a)
        if isClearPathHallway(fromX, toX)
        fromY = 1
        toY = 1 + sideRoomSize - rooms(toX).size
      } yield Move(a, Point(fromX, fromY), Point(toX, toY))

      val sideRoomIntoHall: Set[Move] = for {
        (fromX, as) <- rooms.toSet
        a: Amphipod <- as.headOption.toSet
        toX <- 1 to 11
        if !hall.contains(toX)
        if toX != fromX
        if isClearPathHallway(fromX, toX)
        if !rooms(fromX).forall(
          _.targetSideRoom == fromX
        ) // cannot go out if already in correct pos
        if !Set(A, B, C, D)
          .map(_.targetSideRoom)
          .contains(toX) // cannot stop outside a side room
        toY = 1
        fromY = 2 + sideRoomSize - rooms(fromX).size
      } yield Move(a, Point(fromX, fromY), Point(toX, toY))

      hallIntoSideRoom ++ sideRoomIntoHall
    }

    def applyMove(move: Move): State =
      if (move.from.y == 1) {
        // hall into side room
        copy(
          hall = hall.filter { case (x, _) =>
            x != move.from.x
          },
          rooms = rooms.map { case z @ (x, as) =>
            if (x == move.to.x) (x, move.a :: as)
            else z
          }
        )
      } else {
        // side room into hall
        copy(
          hall = hall.updated(move.to.x, move.a),
          rooms = rooms.map { case z @ (x, as) =>
            if (x == move.from.x) (x, as.tail)
            else z
          }
        )
      }

    def neighs: Set[(State, Int)] = for {
      m <- validMoves
      s = applyMove(m)
    } yield (s, m.expend)
  }

  def dijkstra(initial: State): Int = {
    val ordering = new Ordering[(State, Int)] {
      override def compare(x: (State, Int), y: (State, Int)): Int =
        Integer.compare(y._2, x._2)
    }
    val visited = mutable.Set[State]()
    val pq = mutable.PriorityQueue[(State, Int)]()(ordering)
    val energies = mutable.Map[State, Int]()
    pq.enqueue((initial, 0))
    energies(initial) = 0
    while (pq.nonEmpty) {
      val (v, _) = pq.dequeue()
      val energy = energies(v)
      if (v.isFinal) {
        return energy
      }
      visited += v
      for { (w, deltaE) <- v.neighs if !visited(w) } {
        val newEnergy = energy + deltaE
        if (newEnergy < energies.getOrElse(w, Int.MaxValue)) {
          energies(w) = newEnergy
          pq.enqueue((w, newEnergy))
        }
      }
    }
    -1
  }

  val initialState = State(
    hall = Map(),
    rooms = startPos.groupBy(_._2.x).map { case (x, set) =>
      (x, set.toList.sortBy(_._2.y).map(_._1))
    },
    sideRoomSize = 2
  )

  println(dijkstra(initialState))

  val part2 = initialState.copy(
    rooms = initialState.rooms.map { case (x, as) =>
      x match {
        case 3 => (x, as.head :: List(D, D) ++ as.tail)
        case 5 => (x, as.head :: List(C, B) ++ as.tail)
        case 7 => (x, as.head :: List(B, A) ++ as.tail)
        case 9 => (x, as.head :: List(A, C) ++ as.tail)
      }
    },
    sideRoomSize = 4
  )

  println(dijkstra(part2))
}
