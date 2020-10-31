package aoc2017

import aoc2017.Day12.expandGroup

import scala.annotation.tailrec
import scala.io.Source

object Day12 extends App {

  val input = Source.fromFile("inputs/2017/12.txt")
    .getLines()
    .map(line => line.split(" <-> ")
      .flatMap(part => part.split(", ").toList.map(_.toInt)).toList)
    .map(_.toSet)
    .toList


  @tailrec def expandGroup(grp: Set[Int]): Set[Int] = {
    val added = for {
      elm <- grp
      add <- input(elm) if !grp.contains(add)
    } yield add
    if (added.isEmpty) grp
    else expandGroup(grp ++ added)
  }

  val groupContainingZero = expandGroup(input.head)
  println(groupContainingZero.size)

  @tailrec def countGroups(acc: Int, seen: Set[Int]): Int = {
    if (seen.size == input.length) acc
    else {
      val firstUnseen = input.indices.filter(i => !seen.contains(i)).head
      countGroups(acc + 1, seen ++ expandGroup(Set(firstUnseen)))
    }
  }

  println(countGroups(1, groupContainingZero))
}
