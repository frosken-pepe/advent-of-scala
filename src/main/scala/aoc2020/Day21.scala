package aoc2020

import scala.io.Source
import scala.util.Using

object Day21 extends App {

  type Allergen = String
  type Ingredient = String

  case class Food(ingredients: Set[Ingredient], allergens: Set[Allergen])

  val input = {
    val lines = Using(Source.fromFile("inputs/2020/21.txt"))(_.getLines().toList).get
    val re = """(.*) \(contains (.*)\)""".r
    lines.map {
      case re(ingredients, allergens) => Food(
        ingredients = ingredients.split(" ").toSet,
        allergens = allergens.split(", ").toSet
      )
    }
  }


  case class State(mapping: Map[Allergen, Ingredient], used: Set[Ingredient]) {
    lazy val inverse = mapping.map {
      case (k, v) => v -> k
    }

    def allergens(ingredient: Ingredient): Option[Allergen] = {
      inverse.get(ingredient)
    }
  }

  val allAllergens = input.flatMap(_.allergens).toSet
  val allIngredients = input.flatMap(_.ingredients).toSet

  def assign(state: State, ingredient: Ingredient, allergen: Allergen): Option[State] = {

    val newState = state.copy(mapping = state.mapping.updated(allergen, ingredient), used = state.used + ingredient)

    val mapped = newState.mapping

    val cantBe = input.exists {
      case Food(ingredients, allergens) => allergens.intersect(mapped.keySet).exists(a => !ingredients.contains(mapped(a)))
    }

    if (cantBe) None else Some(newState)
  }

  def possibleIngredientsF(allergen: Allergen): Set[Ingredient] = {
    input.filter(_.allergens.contains(allergen)).flatMap(_.ingredients).toSet
  }

  val possibleIngredients: Map[Allergen, Set[Ingredient]] = allAllergens.map { a =>
    a -> possibleIngredientsF(a)
  }.toMap

  def neighs(state: State): Set[State] = for {
    allergen <- allAllergens
    if !state.mapping.contains(allergen)
    ingredient <- possibleIngredients(allergen)
    if !state.used.contains(ingredient)
    assigned <- assign(state, ingredient, allergen)
  } yield assigned

  val numAllergens = allAllergens.size

  def isFinal(state: State): Boolean = {
    if (state.mapping.size != numAllergens) return false
    input.forall { food => food.allergens.subsetOf(food.ingredients.flatMap(ing => state.allergens(ing))) }
  }

  def search(state: State): Option[State] = {
    if (isFinal(state)) Some(state)
    else {
      for {
        neigh <- neighs(state)
        found <- search(neigh)
      } return Some(found)
      None
    }
  }

  val sol = search(State(Map(), Set()))

  sol.map { state =>
    val ingredients = state.mapping.values
    val safe = allIngredients -- ingredients
    val appears = input.map(food => food.ingredients.count(i => safe.contains(i))).sum
    println(appears)
    val mapped = state.mapping.toList.sorted.map(_._2).mkString(",")
    println(mapped)
  }
}
