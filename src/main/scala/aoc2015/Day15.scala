package aoc2015

import scala.io.Source

object Day15 extends App {

  val targetTeaspoons = 100

  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

  object Ingredient {
    private val ingredient = """(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)""".r

    def apply(s: String): Ingredient = s match {
      case ingredient(na, cap, dur, fla, tex, cal) => new Ingredient(na, cap.toInt, dur.toInt, fla.toInt, tex.toInt, cal.toInt)
    }
  }

  val input = Source.fromFile("inputs/2015/15.txt").getLines()
    .map(Ingredient.apply)
    .toList

  def score(ingredients: Map[Ingredient, Int], targetCalories: Option[Int]): Int = {
    val cap = math.max(0, ingredients.map(it => it._1.capacity * it._2).sum)
    val dur = math.max(0, ingredients.map(it => it._1.durability * it._2).sum)
    val fla = math.max(0, ingredients.map(it => it._1.flavor * it._2).sum)
    val tex = math.max(0, ingredients.map(it => it._1.texture * it._2).sum)
    val cal = math.max(0, ingredients.map(it => it._1.calories * it._2).sum)
    if (targetCalories.forall(_ == cal)) cap * dur * fla * tex else 0
  }

  def partition(no: Int, buckets: Int, done: Int = 0, acc: List[List[Int]] = Nil): List[List[Int]] =
    if (no == 0 && done == buckets) acc
    else if (done == buckets) Nil
    else (0 to no).flatMap(take => partition(no - take, buckets, done + 1,
      if (acc.isEmpty) List(List(take)) else acc.map(take :: _)
    )).toList

  val cakes = partition(targetTeaspoons, input.size)
    .map(_.zipWithIndex.map { case (teaspoons, idx) => input(idx) -> teaspoons }.toMap)

  println(cakes.map(cake => score(cake, None)).max)
  println(cakes.map(cake => score(cake, Some(500))).max)
}
