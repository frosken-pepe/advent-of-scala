package aoc2020

import scala.collection.mutable
import scala.io.Source

object Day23 extends App {

  val input = Source.fromFile("inputs/2020/23.txt").getLines().next().split("").map(_.toInt)

  case class Cup(label: Int, var prev: Cup, var next: Cup) {

    def insertAfter(label: Int): Cup = {
      val oldNext = next
      next = Cup(label, this, oldNext)
      oldNext.prev = next
      next
    }

    def remove: (Int, Cup) = {
      val oldNext = next
      val oldPrev = prev
      oldNext.prev = oldPrev
      oldPrev.next = oldNext
      (label, oldNext)
    }
  }

  def init(): (Cup, mutable.Map[Int, Cup]) = {
    val map = mutable.Map[Int,Cup]()
    var ptr = Cup(input.head, null, null)
    map(input.head) = ptr
    ptr.next = ptr
    ptr.prev = ptr
    for (label <- input.tail) {
      ptr = ptr.insertAfter(label)
      map(label) = ptr
    }
    for (i <- 10 to 1_000_000) {
      ptr = ptr.insertAfter(i)
      map(i) = ptr
    }
    (ptr.next, map)
  }

  val (cups, map) = init()

  def move(current: Cup, cupMap: mutable.Map[Int, Cup]): Cup = {
    val currentLabel = current.label
    val maxLabel = 1_000_000
    val minLabel = 1
    val (lab1, a) = current.next.remove
    val (lab2, b) = a.remove
    val (lab3, c) = b.remove
    var dest = currentLabel - 1
    while (Set(lab1, lab2, lab3).contains(dest)) {
      dest = dest - 1
    }
    if (dest < minLabel) {
      dest = maxLabel
    }
    while (Set(lab1, lab2, lab3).contains(dest)) {
      dest = dest - 1
    }
    var ptr = cupMap(dest)
    ptr = ptr.insertAfter(lab1)
    cupMap(lab1) = ptr
    ptr = ptr.insertAfter(lab2)
    cupMap(lab2) = ptr
    ptr = ptr.insertAfter(lab3)
    cupMap(lab3) = ptr
    current.next
  }

  var current = cups
  for (i <- 1 to 10_000_000) {
    if (i % 1000 == 0) println(s"round $i")
    current = move (current, map)
 }

  while (current.label != 1) {
    current = current.next
  }
  current = current.next

  var prod = 1L
  for (_ <- 1 to 2) {
    prod = prod * current.label
    current = current.next
  }
  println(prod)

}
