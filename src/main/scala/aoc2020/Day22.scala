package aoc2020

import scala.collection.immutable.Queue
import scala.io.Source

object Day22 extends App {

  type Deck = Queue[Int]

  case class State(p1: Deck, p2: Deck, winner: Int, game: Int) {
    def score: Int = {
      val player = (p1.length, p2.length) match {
        case (_, 0) => p1
        case (0, _) => p2
      }
      player.reverse.zip(1 to player.length).map { case (a, b) => a * b }.sum
    }
  }

  val (player1, player2) = {
    val lines = Source.fromFile("inputs/2020/22.txt").getLines().toList
    val player1 = lines.drop(1).takeWhile(_ != "").map(_.toInt)
    val player2 = lines.dropWhile(_ != "Player 2:").drop(1).map(_.toInt)
    (Queue[Int]().enqueueAll(player1), Queue[Int]().enqueueAll(player2))
  }

  def round(state: State, seen: Set[State], maxGame: Int, gameType: Int): (State, Set[State], Int) = {

    if (seen.contains(state)) {
      return (state.copy(winner = 1), seen, maxGame)
    }

    val (hdp1, tlp1) = (state.p1.head, state.p1.tail)
    val (hdp2, tlp2) = (state.p2.head, state.p2.tail)

    val roundWinner =
      if (gameType == 2 && hdp1 <= tlp1.length && hdp2 <= tlp2.length)
        game(State(tlp1.take(hdp1), tlp2.take(hdp2), 0, maxGame + 1), gameType).winner
      else if (hdp1 > hdp2) 1
      else 2

    val nextState = if (roundWinner == 1)
      state.copy(tlp1.enqueue(hdp1).enqueue(hdp2), tlp2, if (tlp2.isEmpty) 1 else 0)
    else
      state.copy(tlp1, tlp2.enqueue(hdp2).enqueue(hdp1), if (tlp1.isEmpty) 2 else 0)

    (nextState, seen + state, math.max(state.game, maxGame))
  }

  def game(init: State, gameType: Int): State = {
    LazyList.iterate((init, Set[State](), 0)) {
      case (state, seen, maxGame) => round(state, seen, maxGame, gameType)
    }.map(_._1).dropWhile(state => state.winner == 0).head
  }

  println(game(State(player1, player2, 0, 0), 1).score)
  println(game(State(player1, player2, 0, 0), 2).score)
}
