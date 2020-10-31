package aoc2016

import scala.collection.mutable
import scala.io.Source

object Day22 extends App {

  case class Node(x: Int, y: Int, size: Int, used: Int) {
    def avail: Int = size - used
  }

  val node = """/dev/grid/node-x(\d+)-y(\d+) +(\d+)T +(\d+)T.*""".r

  val input = Source.fromFile("inputs/2016/22.txt").getLines().flatMap(_ match {
    case node(x, y, size, used) => Some(Node(x.toInt, y.toInt, size.toInt, used.toInt))
    case _ => None
  }).toList

  val viablePairs = (for {
    a <- input if a.used != 0
    b <- input if b != a && b.avail >= a.used
  } yield ()).size

  case class State(data: (Int, Int), used: Map[(Int, Int), Int], size: Map[(Int, Int), Int]) {
    def avail(key: (Int, Int)): Int = size(key) - used(key)

    def manhattan: Int = data._1 + data._2
  }

  def createInitialState(input: List[Node]): State = {
    val dataNode = input.filter(_.y == 0).maxBy(_.x)
    val used = input.foldLeft(Map[(Int, Int), Int]()) {
      case (acc, item) => acc.updated((item.x, item.y), item.used)
    }
    val size = input.foldLeft(Map[(Int, Int), Int]()) {
      case (acc, item) => acc.updated((item.x, item.y), item.size)
    }
    State((dataNode.x, dataNode.y), used, size)
  }

  def neighs(state: State): Set[State] = {
    val keys = state.used.keys.toSet
    for {
      src <- keys
      tgt <- List((src._1 - 1, src._2), (src._1 + 1, src._2), (src._1, src._2 - 1), (src._1, src._2 + 1))
      if keys.contains(tgt) && state.avail(tgt) >= state.used(src)
      newDataKey = if (src == state.data) tgt else state.data
      newUsed = state.used.updated(src, 0).updated(tgt, state.used(tgt) + state.used(src))
    } yield State(newDataKey, newUsed, state.size)
  }

  def isFinal(state: State) = state.data == (0, 0)

  val initial = createInitialState(input)

  implicit val ordering: Ordering[(Int, State)] = (x: (Int, State), y: (Int, State)) =>
    -Integer.compare( -10 * x._2.manhattan +  x._1,  -10 * y._2.manhattan +  y._1)

  def search(state: State): Int = {
    val q = mutable.PriorityQueue[(Int, State)]()
    val visited = mutable.Set[State]()
    q.enqueue((0, state))
    visited.add(state)
    while (q.nonEmpty) {
      val (dist, curr) = q.dequeue()
      if (isFinal(curr)) return dist
      else for {n <- neighs(curr)} {
        if (!visited.contains(n)) {
          visited.add(n)
          q.enqueue((dist + 1, n))
        }
      }
    }
    -1
  }

  println(search(createInitialState(input)))
  // TODO
}
