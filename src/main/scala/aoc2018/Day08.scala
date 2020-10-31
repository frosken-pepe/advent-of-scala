package aoc2018

import scala.annotation.tailrec
import scala.io.Source

object Day08 extends App {

  val input = Source.fromFile("inputs/2018/08.txt").getLines().next().split(" ").map(_.toInt).toList

  case class Node(children: List[Node], metadata: List[Int]) {
    def value: Int =
      if (children.isEmpty) metadata.sum
      else metadata.map(_ - 1).filter(children.indices.contains).map(children).map(_.value).sum
  }

  @tailrec def many[T](fn: List[Int] => (T, List[Int]))
                      (rest: List[Int], n: Int, acc: List[T] = Nil): (List[T], List[Int]) = {
    if (n == 0) (acc.reverse, rest)
    else {
      val (item, rst) = fn(rest)
      many(fn)(rst, n - 1, item :: acc)
    }
  }

  def node(l: List[Int]): (Node, List[Int]) = {
    val nNode :: nMeta :: r0 = l
    val (children, r1) = many(node)(r0, nNode)
    val (metadata, r2) = many(z => (z.head, z.tail))(r1, nMeta)
    (Node(children, metadata), r2)
  }

  val root = node(input)._1

  def sumMeta(node: Node): Int = {
    node.metadata.sum + node.children.map(sumMeta).sum
  }

  println(sumMeta(root))

  println(root.value)
}
