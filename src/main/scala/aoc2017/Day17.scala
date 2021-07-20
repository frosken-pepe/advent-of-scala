package aoc2017

import scala.io.Source
import scala.util.Using

object Day17 extends App {

  val input = Using(Source.fromFile("inputs/2017/17.txt"))(_.getLines().next().toInt).get

  case class Node(value: Int, var next: Node)

  private def part1(): Unit = {
    val initialNode = Node(0, null)
    initialNode.next = initialNode
    var ptr = initialNode
    for {i <- 1 to 2017} {
      for {_ <- 1 to input} ptr = ptr.next
      val newNode = Node(i, ptr.next)
      ptr.next = newNode
      ptr = newNode
      if (newNode.value == 2017) println(newNode.next.value)
    }
  }
  part1()

  private def part2(): Unit = {
    val initialNode = Node(0, null)
    initialNode.next = initialNode
    var ptr = initialNode
    for {i <- 1 to 50_000_000} {
      for {_ <- 1 to input} ptr = ptr.next
      val newNode = Node(i, ptr.next)
      ptr.next = newNode
      ptr = newNode
    }
    while (ptr.value != 0) ptr = ptr.next
    println(ptr.next.value)
  }
  part2()
}
