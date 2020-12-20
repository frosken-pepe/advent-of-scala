package aoc2020

import scala.io.Source

object Day20 extends App {

  case class Tile(id: Long, pixels: Set[(Int, Int)]) {

    val maxX: Int = pixels.map(_._1).max
    val maxY: Int = pixels.map(_._2).max

    def rotate(): Tile = {
      copy(pixels = pixels.map {
        case (x, y) => (maxY - y, x)
      })
    }

    def rotateN(n: Int): Tile = {
      if (n == 0) this
      else this.rotate().rotateN(n - 1)
    }

    def flipH: Tile = copy(pixels = pixels.map {
      case (x, y) => (maxX - x, y)
    })

    def flipV: Tile = copy(pixels = pixels.map {
      case (x, y) => (x, maxY - y)
    })

    lazy val variations: Set[Tile] = for {
      flipped <- Set(this, flipH, flipV)
      rotations <- 0 until 4
      rotated = flipped.rotateN(rotations)
    } yield rotated
  }

  object Tile {
    def apply(id: Long, strings: List[String]): Tile = new Tile(id,
      for {
        (line, y) <- strings.zipWithIndex.toSet
        (ch, x) <- line.zipWithIndex if ch == '#'
      } yield (x, y)
    )

    val seaMonster: Tile = Tile(666, List(
      "                  # ",
      "#    ##    ##    ###",
      " #  #  #  #  #  #   ",
    ))
  }

  val input = {
    val lines = Source.fromFile("inputs/2020/20.txt").getLines().mkString("\n").split("\n\n").toList

    def parseTile(tile: List[String]): Tile = {
      val re = """Tile (\d+):""".r
      val id = tile.head match {
        case re(id) => id.toInt
      }
      Tile(id, tile.tail)
    }

    lines.map(line => parseTile(line.split("\n").toList)).toSet
  }

  def matchLeft(left: Tile, right: Tile): Boolean = {
    (0 to left.maxX).forall(y => left.pixels(left.maxX, y) == right.pixels(0, y))
  }

  def matchNorth(up: Tile, down: Tile): Boolean = {
    (0 to down.maxY).forall(x => up.pixels(x, down.maxY) == down.pixels(x, 0))
  }

  def neighs(leftNeigh: Option[Tile], northNeigh: Option[Tile], used: Set[Long]): Set[Tile] = {
    for {
      tile <- input
      if !used.contains(tile.id)
      rotatedTile <- tile.variations
      if leftNeigh.isEmpty || matchLeft(leftNeigh.get, rotatedTile)
      if northNeigh.isEmpty || matchNorth(northNeigh.get, rotatedTile)
    } yield rotatedTile
  }

  val rows = math.round(math.sqrt(input.size)).toInt
  val cols = rows

  def roughness(tiles: Map[(Int, Int), Tile]): Int = {
    val tile = Tile(-1, assembleImage(tiles))
    val hereBeDragons = for {
      monster <- Tile.seaMonster.variations
      monsterPixel <- overlappingPixels(monster, tile)
    } yield monsterPixel
    tile.pixels.count(px => !hereBeDragons.contains(px))
  }

  private def assembleImage(tiles: Map[(Int, Int), Tile]): Set[(Int, Int)] = {
    val shifted = for {
      row: Int <- (0 until rows).toSet
      col <- 0 until cols
      tile = tiles(row, col)
      offsetX = (tile.maxX - 1) * col - 1
      offsetY = (tile.maxY - 1) * row - 1
      shiftedPixels = tile.pixels.filter {
        case (0, _) => false
        case (_, 0) => false
        case (x, _) if x == tile.maxX => false
        case (_, y) if y == tile.maxY => false
        case _ => true
      }.map {
        case (x, y) => (x + offsetX, y + offsetY)
      }
    } yield shiftedPixels
    shifted.flatten
  }

  def overlappingPixels(monster: Tile, tile: Tile): Set[(Int, Int)] = {
    (for {
      x <- 0 to tile.maxX - monster.maxX
      y <- 0 to tile.maxY - monster.maxY
      shiftedMonster = monster.pixels.map { case (mx, my) => (x + mx, y + my) }
      if shiftedMonster.forall(tile.pixels.contains)
      monsterPixel <- shiftedMonster
    } yield monsterPixel).toSet
  }

  def solve(row: Int, col: Int, acc: Map[(Int, Int), Tile]): Option[Map[(Int, Int), Tile]] = {
    if (acc.size == input.size) Some(acc)
    else {
      val leftNeigh = if (col > 0) Some(acc(row, col - 1)) else None
      val northNeigh = if (row > 0) Some(acc(row - 1, col)) else None
      for {
        n <- neighs(leftNeigh, northNeigh, acc.values.map(_.id).toSet)
        sol <- solve(
          if (col == cols - 1) row + 1 else row,
          if (col == cols - 1) 0 else col + 1,
          acc.updated((row, col), n))
      } return Some(sol)
      None
    }
  }

  val sol = solve(0, 0, Map()).get

  println(sol(0, 0).id * sol(0, cols - 1).id * sol(rows - 1, 0).id * sol(rows - 1, cols - 1).id)

  println(roughness(sol))
}
