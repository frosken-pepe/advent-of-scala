package aoc2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day13 extends App {

  // TODO cleanup

  val input = Using(Source.fromFile("inputs/2018/13.txt"))(_.getLines().toList).get

  val carts = (for {
    y <- input.indices
    x <- input(y).indices
    ch = input(y)(x)
    if Set('^', '<', 'v', '>') contains ch
  } yield Cart(x, y, ch, R)).toList

  val map = for {
    line <- input
    mapped = line.map {
      case '^' | 'v' => '|'
      case '<' | '>' => '-'
      case x => x
    }
  } yield mapped

  sealed trait Heading {
    def next: Heading
  }

  case object L extends Heading {
    def next: Heading = S
  }

  case object S extends Heading {
    def next: Heading = R
  }

  case object R extends Heading {
    def next: Heading = L
  }

  case class Cart(x: Int, y: Int, dir: Char, lastTurn: Heading) {

    def coords: (Int, Int) = (x, y)

    private def forward: Cart = dir match {
      case '^' => copy(y = y - 1)
      case '<' => copy(x = x - 1)
      case 'v' => copy(y = y + 1)
      case '>' => copy(x = x + 1)
    }

    private def turn: Cart = ((dir, lastTurn.next) match {
      case ('^', L) => copy(dir = '<')
      case ('<', L) => copy(dir = 'v')
      case ('v', L) => copy(dir = '>')
      case ('>', L) => copy(dir = '^')

      case (_, S) => this

      case ('^', R) => copy(dir = '>')
      case ('<', R) => copy(dir = '^')
      case ('v', R) => copy(dir = '<')
      case ('>', R) => copy(dir = 'v')
    }).copy(lastTurn = lastTurn.next)

    def move(map: List[String]): Cart = ((dir, map(y)(x)) match {
      case ('^', '\\') => copy(dir = '<')
      case ('<', '\\') => copy(dir = '^')
      case ('v', '\\') => copy(dir = '>')
      case ('>', '\\') => copy(dir = 'v')

      case ('^', '/') => copy(dir = '>')
      case ('<', '/') => copy(dir = 'v')
      case ('v', '/') => copy(dir = '<')
      case ('>', '/') => copy(dir = '^')

      case (_, '+') => turn
      case (_, _) => this
    }).forward
  }

  def sortCarts(carts: List[Cart]): List[Cart] = carts.sortBy(_.coords.swap)

  sealed trait Tick

  case class Crashed(loc: (Int, Int)) extends Tick

  case class Moved(carts: List[Cart]) extends Tick

  {
    @tailrec def moveAll(todo: List[Cart], moved: List[Cart]): Tick = {
      if (todo.isEmpty) Moved(moved)
      else {
        val newCart = todo.head.move(map)
        val cartLocations = (todo.tail ++ moved).map(_.coords).toSet
        if (cartLocations.contains(newCart.coords)) Crashed(newCart.coords)
        else moveAll(todo.tail, newCart :: moved)
      }
    }

    @tailrec def firstCollision(carts: List[Cart]): Crashed = moveAll(sortCarts(carts), Nil) match {
      case c@Crashed(_) => c
      case m@Moved(_) => firstCollision(m.carts)
    }

    println(firstCollision(carts).loc)
  }

  {
    def crashed(newCart: Cart, todo: List[Cart], moved: List[Cart]): List[Cart] = {
      moveAll(todo.tail.filter(cart => cart.coords != newCart.coords),
        moved.filter(cart => cart.coords != newCart.coords))
    }

    @tailrec def moveAll(todo: List[Cart], moved: List[Cart]): List[Cart] = {
      if (todo.isEmpty) moved
      else {
        val newCart = todo.head.move(map)
        val cartLocations = (todo.tail ++ moved).map(_.coords).toSet
        if (cartLocations.contains(newCart.coords)) crashed(newCart, todo, moved)
        else moveAll(todo.tail, newCart :: moved)
      }
    }

    @tailrec def lastRemaining(carts: List[Cart]): (Int, Int) = {
      if (carts.size == 1) carts.head.coords
      else lastRemaining(moveAll(sortCarts(carts), Nil))
    }

    println(lastRemaining(carts))
  }
}
