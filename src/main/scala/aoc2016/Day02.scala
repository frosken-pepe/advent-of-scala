package aoc2016

import scala.io.Source

object Day02 extends App {

  val input = Source.fromFile("inputs/2016/02.txt").getLines().toList

  def nextButton(lay: List[List[Char]])(p: (Int, Int), str: String) = {
    def next(pos: (Int, Int), ch: Char) = (pos, ch) match {
      case (pos, 'U') => (pos._1, pos._2 - 1)
      case (pos, 'D') => (pos._1, pos._2 + 1)
      case (pos, 'L') => (pos._1 - 1, pos._2)
      case (pos, 'R') => (pos._1 + 1, pos._2)
    }

    def hasButton(pos: (Int, Int)): Boolean =
      pos._1 >= 0 && pos._2 >= 0 && pos._1 < lay.head.size && pos._2 < lay.size && lay(pos._2)(pos._1) != ' '

    str.foldLeft(p) {
      case (pos, ch) if hasButton(next(pos, ch)) => next(pos, ch)
      case (pos, _) => pos
    }
  }

  def find(ch: Char, lay: List[List[Char]]): (Int, Int) = (for {
    (list, y) <- lay.zipWithIndex
    (c, x) <- list.zipWithIndex if c == ch
  } yield (x,y)).head

  def password(lay: List[List[Char]]): String =
    input.scanLeft(find('5', lay))(nextButton(lay)).drop(1).map(q => lay(q._2)(q._1)).mkString

  val layoutP1 = List(
    List('1', '2', '3'),
    List('4', '5', '6'),
    List('7', '8', '9'),
  )

  val layoutP2 = List(
    List(' ', ' ', '1', ' ', ' '),
    List(' ', '2', '3', '4', ' '),
    List('5', '6', '7', '8', '9'),
    List(' ', 'A', 'B', 'C', ' '),
    List(' ', ' ', 'D', ' ', ' '),
  )

  println(password(layoutP1))
  println(password(layoutP2))
}
