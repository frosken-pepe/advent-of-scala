package aoc2020

import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.io.Source

object Day22 extends App {

  type Deck = Queue[Int]

  case class State(p1: Deck, p2: Deck, end: Boolean, win: Int) {
    def winner = if (p1.isEmpty) p2 else p1

    def score: Int = winner.reverse.zip(1 to winner.length).map {
      case (a, b) => a * b
    }.sum
  }

  val (player1, player2) = {
    val lines = Source.fromFile("inputs/2020/22.txt").getLines().toList
    val player1 = lines.drop(1).takeWhile(_ != "").map(_.toInt)
    val player2 = lines.dropWhile(_ != "Player 2:").drop(1).map(_.toInt)
    (Queue[Int]().enqueueAll(player1), Queue[Int]().enqueueAll(player2))
  }

  val cache = mutable.Map[Int, mutable.Set[State]]()

  def round(state: State, game: Int): State = {

    if (!cache.contains(game)) cache(game) = mutable.Set[State]()

    if (cache(game).contains(state)) {
      return state.copy(end = true, win = 1)
    }

    cache(game).add(state)

    val (hdp1, tlp1) = (state.p1.head, state.p1.tail)
    val (hdp2, tlp2) = (state.p2.head, state.p2.tail)

    val roundWinner = if (hdp1 <= tlp1.length && hdp2 <= tlp2.length) {
      recursiveCombat(tlp1.take(hdp1), tlp2.take(hdp2), cache.keys.max + 1)
    } else {
      if (hdp1 > hdp2) 1 else 2
    }

    if (roundWinner == 1)
      state.copy(tlp1.enqueue(hdp1).enqueue(hdp2), tlp2, tlp2.isEmpty, if (tlp2.isEmpty) 1 else 0)
    else
      state.copy(tlp1, tlp2.enqueue(hdp2).enqueue(hdp1), tlp1.isEmpty, if (tlp1.isEmpty) 2 else 0)
  }

  def recursiveCombat(p1: Deck, p2: Deck, game: Int): Int = {
    val state = State(p1, p2, false, 0)
    LazyList.iterate(state) { state =>
      round(state, game)
    }.dropWhile(s => !s.end).head.win
  }

  val init = State(player1, player2, false, 0)

  var round = 0

  println(LazyList.iterate(init) { state =>
    println(s"round $round")
    round += 1
    round(state, 0)
  }
    .dropWhile { state => !state.end }.map(_.score).head)
}
