package aoc2021

import scala.collection.mutable

object Day21 extends App {

  val maxScore = 21

  case class GameState(p1pos: Int, p2pos: Int, p1score: Int, p2score: Int, isPlayer1Turn: Boolean, diesRolled: Int, sumRolls: Int) {

    def updatePos(p: Int, rolled: Int): Int = {
      val temp = (p + rolled) % 10
      if (temp == 0) 10 else temp
    }

    def advance: List[GameState] = {
      if (diesRolled < 3) List(1, 2, 3).map(roll => copy(sumRolls = sumRolls + roll, diesRolled = diesRolled + 1))
      else {
        val newPos = updatePos(if (isPlayer1Turn) p1pos else p2pos, sumRolls)
        if (isPlayer1Turn) List(copy(p1pos = newPos, p1score = p1score + newPos, diesRolled = 0, sumRolls = 0, isPlayer1Turn = false))
        else List(copy(p2pos = newPos, p2score = p2score + newPos, diesRolled = 0, sumRolls = 0, isPlayer1Turn = true))
      }
    }

    def isFinal: Boolean = p1score >= maxScore || p2score >= maxScore

    def winningPlayer: Int = if (p1score >= maxScore) 1 else 2
  }


  val cache = mutable.Map[GameState, (Long, Long)]()

  def recur(state: GameState): (Long, Long) = {
    if (cache.contains(state)) cache(state)
    else {
      val (finished, ongoing) = state.advance.partition(_.isFinal)
      val (p1won, p2won) = finished.partition(_.winningPlayer == 1)
      val recurred = ongoing.map(recur)
      val res = (p1won.size + recurred.map(_._1).sum, p2won.size + recurred.map(_._2).sum)
      cache(state) = res
      res
    }
  }

  val initialState = GameState(2, 10, 0, 0, isPlayer1Turn = true, 0, 0)
  val res = recur(initialState)
  println(if (res._1 > res._2) res._1 else res._2)
}
