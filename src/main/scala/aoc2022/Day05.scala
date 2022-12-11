package aoc2022

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day05 extends App {

  private val input: List[String] = Using(Source.fromFile("inputs/2022/05.txt"))(_.getLines().toList).get

  private def makeStack(input: List[Char]): Option[List[Char]] = {
    if (input.exists(ch => '1' <= ch && ch <= '9')) Some(input.filter(ch => 'A' <= ch && ch <= 'Z'))
    else None
  }

  private val stacks: List[List[Char]] = input.takeWhile(_.nonEmpty).transpose.flatMap(makeStack)

  case class Move(amt: Int, from: Int, to: Int)

  val moves = input.dropWhile(_.nonEmpty).tail.map {
    case s"move $a from $f to $t" => Move(a.toInt, f.toInt - 1, t.toInt - 1)
  }

  @tailrec
  private def moveSingle(stacks: List[List[Char]], move: Move): List[List[Char]] = move match {
    case Move(0, _, _) => stacks
    case Move(a, f, t) => moveSingle(moveMultiple(stacks, Move(1, f, t)), move.copy(amt = a - 1, f, t))
  }

  println(moves.foldLeft(stacks) { case (stacks, move) => moveSingle(stacks, move)}.map(_.head).mkString)

  private def moveMultiple(stacks: List[List[Char]], move: Move): List[List[Char]] = {
    stacks.zipWithIndex.map {
      case (stack, idx) if idx == move.from => stack.drop(move.amt)
      case (stack, idx) if idx == move.to => stacks(move.from).take(move.amt) ++ stack
      case (stack, _) => stack
    }
  }

  println(moves.foldLeft(stacks) { case (stacks, move) => moveMultiple(stacks, move)}.map(_.head).mkString)
}
