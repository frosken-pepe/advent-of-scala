package aoc2016

import scala.collection.mutable
import scala.io.Source

object Day11 extends App {

  val english = Map(
    "first" -> 1, "second" -> 2, "third" -> 3, "fourth" -> 4
  )

  val floor = """The (\w+) floor contains (.*)\.""".r
  val generator = """a (\w+) generator""".r
  val microchip = """a (\w+)-compatible microchip""".r

  sealed abstract class Item(val name: String) {
    def isChip: Boolean
    final def isGenerator: Boolean = !isChip
  }

  case class Generator(tp: String) extends Item(tp) {
    override def isChip: Boolean = false
  }

  case class Chip(tp: String) extends Item(tp) {
    override def isChip: Boolean = true
  }

  def parseItem(s: String): Option[Item] = s match {
    case generator(s) => Some(Generator(s))
    case microchip(s) => Some(Chip(s))
    case "nothing relevant" => None
  }

  val input = Source.fromFile("inputs/2016/11.txt").getLines()
    .map {
      case floor(a, b) => (english(a) - 1, b.split(", and |, | and ").flatMap(parseItem).toList)
    }.toList.sortBy(_._1).map(_._2)

  case class State(elevator: Int, floors: List[List[Item]])

  def legalFloor(items: List[Item]): Boolean = {
    val generators = items.filter(_.isGenerator).map(_.name)
    val chips = items.filter(_.isChip).map(_.name)
    generators.isEmpty || chips.forall(generators.contains)
  }

  def legalState(state: State): Boolean = state.floors.forall(legalFloor)

  def move[T](from: Int, to: Int, floors: List[List[T]], moved: List[T]): List[List[T]] = {
    floors.zipWithIndex.map {
      case (floor, `from`) => floor.filter(item => !moved.contains(item))
      case (floor, `to`) => floor ++ moved
      case (floor, _) => floor
    }
  }

  def subsets[T](list: List[T], n: Int): Seq[List[T]] = {
    if (n == 0 || list.isEmpty || n > list.size) List(List())
    else for {
      i <- 0 until list.size - n + 1
      rest <- subsets(list.drop(i + 1), n - 1)
    } yield list(i) :: rest
  }

  def neighs(state: State): List[State] = {
    val e = state.elevator
    for {
      nextE <- List(e - 1, e + 1) if nextE >= 0 && nextE < state.floors.size
      numItems <- 1 to math.min(2, state.floors(e).size)
      movedItems <- subsets(state.floors(e), numItems)
      neigh = State(nextE, move(e, nextE, state.floors, movedItems)) if legalState(neigh)
    } yield neigh
  }

  def problem(input: List[List[Item]]): Int = {

    def eqClass(state: State): String = {
      val (floors, elevator) = (state.floors, state.elevator)
      val objects = (0 until (state.floors.map(_.size).sum) / 2).map(_ => Array(0, 0)).toList
      val names = mutable.Map[String, Int]()
      for {
        i <- floors.indices
        qq <- floors(i)
        j = if (names.contains(qq.name)) names(qq.name)
        else {
          names(qq.name) = names.size
          names(qq.name)
        }
      } objects(j)(if (qq.isGenerator) 0 else 1) = i
      objects.map(_.mkString(",")).sorted.mkString(";") + ";" + elevator
    }

    val target: State => Boolean = state => {
      state.floors.take(input.size - 1).forall(_.isEmpty)
    }

    def bfs(state: State): Int = {
      val queue = mutable.Queue[(Int, State)]()
      val visited = mutable.Set[String]()
      visited.add(eqClass(state))
      queue.enqueue((0, state))

      while (queue.nonEmpty) {
        val (dist, current) = queue.dequeue()
        if (target(current)) return dist
        for {neigh <- neighs(current)} {
          val key = eqClass(neigh)
          if (!visited.contains(key)) {
            visited.add(key)
            queue.enqueue((dist + 1, neigh))
          }
        }
      }
      -1
    }

    bfs(State(0, input))
  }

  println(problem(input))

  println(problem(input.zipWithIndex.map {
    case (floor, 0) => floor ++ List(Generator("elerium"), Chip("elerium"), Generator("dilithium"), Chip("dilithium"))
    case (floor, _) => floor
  }))
}
