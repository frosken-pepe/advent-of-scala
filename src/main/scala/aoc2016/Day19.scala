package aoc2016

import scala.annotation.tailrec
import scala.io.Source

object Day19 extends App {

  val input = Source.fromFile("inputs/2016/19.txt").getLines().next().toInt

  case class Node(id: Int, var next: Node)

  def createCircle(n: Int): Node = {
    val head = Node(1, null)
    var cur = head
    for {p <- 2 to n} {
      cur.next = Node(p, null)
      cur = cur.next
    }
    cur.next = head
    head
  }

  @tailrec def stealingPresents(circle: Node, n: Int): Int = {
    if (n == 1) circle.id
    else {
      circle.next = circle.next.next
      stealingPresents(circle.next, n - 1)
    }
  }

  println(stealingPresents(createCircle(input), input))

  @tailrec def stealingPresentsP2(circle: Node, beforeTarget: Node, n: Int): Int = {
    if (n == 1) circle.id
    else {
      beforeTarget.next = beforeTarget.next.next
      stealingPresentsP2(circle.next, if (n % 2 == 0) beforeTarget else beforeTarget.next, n - 1)
    }
  }

  @tailrec def advance(node: Node, n: Int): Node =
    if (n == 0) node else advance(node.next, n - 1)

  val c = createCircle(input)

  println(stealingPresentsP2(c, advance(c, input / 2 - 1), input))
}
