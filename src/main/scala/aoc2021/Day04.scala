package aoc2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day04 extends App {

  case class Board(numbers: List[List[Int]]) {
    lazy val T: Board = Board(numbers.transpose)
  }

  case class Game(numbers: List[Int], boards: List[Board], played: List[Int])

  val input: Game = Using(Source.fromFile("inputs/2021/04.txt")) { source =>
    val lines = source.getLines().toList
    val numbers = lines.head.split(",").map(_.toInt).toList
    val boards = lines.drop(2).sliding(6, 6).map { window =>
      Board(window.filter(_.nonEmpty).map(_.split("\\s+").filter(_.nonEmpty).map(_.toInt).toList))
    }.toList
    Game(numbers, boards, Nil)
  }.get

  def score(board: Board, played: List[Int]): Int = {
    val sumUnmarked = board.numbers.flatten.filter(!played.contains(_)).sum
    sumUnmarked * played.head
  }

  def checkRows(board: Board, played: List[Int]): Option[Int] = {
    board.numbers.find(_.forall(played.contains(_))).map(_ => score(board, played))
  }

  def checkBoard(board: Board, played: List[Int]): Option[Int] = {
    checkRows(board, played).orElse(checkRows(board.T, played))
  }

  def check(game: Game): Option[Int] = {
    game.boards.flatMap(board => checkBoard(board, game.played)).headOption
  }

  @tailrec def part1(game: Game): Int = {
    val newGame = game.copy(played = game.numbers.head :: game.played, numbers = game.numbers.tail)
    val checked = check(newGame)
    if (checked.isDefined) checked.get
    else part1(newGame)
  }

  println(part1(input))

  def removeWinningBoards(game: Game): Game = {
    game.copy(boards = game.boards.filter(checkBoard(_, game.played).isEmpty))
  }

  @tailrec def part2(game: Game): Int = {
    val newGame =
      removeWinningBoards(game.copy(played = game.numbers.head :: game.played, numbers = game.numbers.tail))
    if (newGame.boards.size == 1) part1(newGame)
    else part2(newGame)
  }

  println(part2(input))
}
