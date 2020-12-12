package aoc2018

import scala.io.Source

object Day14 extends App {

  // piece of crap

  val input = Source.fromFile("inputs/2018/14.txt").getLines().next()

  case class Recipe(score: Int, var next: Recipe) {
    def insertAfter(s: Int): Recipe = {
      val newR = Recipe(s, this.next)
      this.next = newR
      newR
    }
  }

  case class State(var elf1: Recipe, var elf2: Recipe, var eol: Recipe)

  def init(): State = {
    val r3 = Recipe(3, null)
    val r7 = Recipe(7, r3)
    r3.next = r7
    State(r3, r7, r7)
  }

  def generateRecipes(state: State): List[Int] = {
    val sum = state.elf1.score + state.elf2.score
    val newScores = sum.toString.split("").map(_.toInt).toList
    for (s <- newScores) {
      state.eol = state.eol.insertAfter(s)
    }
    newScores
  }

  def move(elf: Recipe): Recipe = {
    var ptr = elf
    for (_ <- 0 until elf.score + 1) ptr = ptr.next
    ptr
  }

  def p1(state: State): String = {
    val inputInt = input.toInt
    var generated = 0
    while (true) {
      generated += generateRecipes(state).size
      if (generated > inputInt + 10) {
        var ptr = state.eol.next
        for (_ <- 0 until inputInt) {
          ptr = ptr.next
        }
        val sb = new StringBuilder()
        for (_ <- 0 until 10) {
          sb.append(ptr.score)
          ptr = ptr.next
        }
        return sb.toString()
      }
      state.elf1 = move(state.elf1)
      state.elf2 = move(state.elf2)
    }
    throw new IllegalStateException()
  }

  println(p1(init()))

  def p2(state: State): Int = {
    val seqLen = input.length
    var seq = (0 until seqLen - 2).map(_ => '_').mkString + "37"
    var left = -seq.length + 2 // 2 initial recipes
    while (true) {
      for (g <- generateRecipes(state)) {
        seq = seq.substring(1) + g
        left += 1
        if (seq == input) {
          return left
        }
      }
      state.elf1 = move(state.elf1)
      state.elf2 = move(state.elf2)
    }
    throw new IllegalStateException()
  }

  println(p2(init()))
}
