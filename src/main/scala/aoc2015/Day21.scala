package aoc2015

import scala.io.Source
import scala.util.Using

object Day21 extends App {

  val input = Using(Source.fromFile("inputs/2015/21.txt"))(_.getLines()
    .map(line => line.split(": "))
    .map(arr => arr(0) -> arr(1).toInt)
    .toMap).get

  case class Stats(hp: Int, dmg: Int, armor: Int)

  val boss = Stats(input("Hit Points"), input("Damage"), input("Armor"))

  def damageDealt(attacker: Stats, defender: Stats): Int = math.max(1, attacker.dmg - defender.armor)

  case class StoreItem(cost: Int, dmg: Int, armor: Int)

  object Store {
    // cost, damage, armor
    val weapons: Map[String, StoreItem] = Map(
      "Dagger" -> StoreItem(8, 4, 0),
      "Shortsword" -> StoreItem(10, 5, 0),
      "Warhammer" -> StoreItem(25, 6, 0),
      "Longsword" -> StoreItem(40, 7, 0),
      "Greataxe" -> StoreItem(74, 8, 0),
    )
    val armor: Map[String, StoreItem] = Map(
      "Leather" -> StoreItem(13, 0, 1),
      "Chainmail" -> StoreItem(31, 0, 2),
      "Splintmail" -> StoreItem(53, 0, 3),
      "Bandedmail" -> StoreItem(75, 0, 4),
      "Platemail" -> StoreItem(102, 0, 5),
    )
    val rings: Map[String, StoreItem] = Map(
      "Damage +1" -> StoreItem(25, 1, 0),
      "Damage +2" -> StoreItem(50, 2, 0),
      "Damage +3" -> StoreItem(100, 3, 0),
      "Defense +1" -> StoreItem(20, 0, 1),
      "Defense +2" -> StoreItem(40, 0, 2),
      "Defense +3" -> StoreItem(80, 0, 3),
    )
  }

  case class Equipment(weapons: List[String], armor: List[String], rings: List[String]) {
    private def sumBy(f: StoreItem => Int): Int =
      weapons.map(Store.weapons).map(f).sum +
        armor.map(Store.armor).map(f).sum +
        rings.map(Store.rings).map(f).sum

    def sumCost: Int = sumBy(_.cost)

    def stats: Stats = Stats(100, sumBy(_.dmg), sumBy(_.armor))
  }

  val equipments = (for {
    weapon <- Store.weapons.keys
    weaponList = List(weapon)
    armor <- Store.armor.keys
    armorList <- List(Nil, List(armor))
    ring1 <- Store.rings.keys
    ring2 <- Store.rings.keys if ring2 != ring1
    ringList <- List(Nil, List(ring1), List(ring1, ring2))
  } yield Equipment(weaponList, armorList, ringList)).toSet

  def doesPlayerWin(player: Stats) = LazyList.iterate((0, player, boss)) {
    case (turn, attacker, defender) => (turn + 1, defender.copy(hp = defender.hp - damageDealt(attacker, defender)), attacker)
  }.dropWhile(_._2.hp > 0).head._1 % 2 == 1

  println(equipments.filter(equip => doesPlayerWin(equip.stats)).map(_.sumCost).min)
  println(equipments.filter(equip => !doesPlayerWin(equip.stats)).map(_.sumCost).max)
}
