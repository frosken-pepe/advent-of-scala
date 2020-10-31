package aoc2017

import scala.io.Source

object Day07 extends App {

  case class Program(name: String, weight: Int, children: Set[String])

  val program = """(\w+) \((\d+)\)( -> )?(.*?)""".r

  val input = Source.fromFile("inputs/2017/07.txt")
    .getLines()
    .map { case program(name, weight, _, rest) => Program(name, weight.toInt, if (!rest.isEmpty) rest.split(", ").toSet else Set()) }
    .map(p => (p.name, p))
    .toMap

  val parents = input.values.foldLeft(Map[String, String]()) {
    case (acc, item) => item.children.foldLeft(acc) {
      case (acc2, child) => acc2.updated(child, item.name)
    }
  }

  val bottom = LazyList.unfold(input.head._1) { child =>
    parents.get(child).map(x => (x, x))
  }.last

  println(bottom)

  def weight(name: String): Int = {
    val children = input(name).children
    input(name).weight + children.toList.map(weight).sum
  }

  def isBalanced(name: String): Boolean = {
    input(name).children.isEmpty || input(name).children.map(weight).size == 1
  }

  val unbalancedParent = input.keys.filter(key => !isBalanced(key) && input(key).children.forall(isBalanced)).head

  val motherfucker = input(unbalancedParent)
    .children
    .map(child => (child, weight(child)))
    .groupBy(_._2)
    .map { case (k, v) => (k, v.size) }
    .minBy(_._2)
    ._1

  val unbalancedChild = input(unbalancedParent).children.filter(child => weight(child) == motherfucker).head

  val balancedChild = input(unbalancedParent).children.filter(ch => ch != unbalancedChild).head

  println(weight(balancedChild) - input(unbalancedChild).children.toList.map(weight).sum)
}
