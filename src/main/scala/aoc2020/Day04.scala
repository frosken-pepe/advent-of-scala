package aoc2020

import scala.io.Source
import scala.util.Using

object Day04 extends App {

  val passports = Using(Source.fromFile("inputs/2020/04.txt"))(_.getLines()
    .toList
    .mkString("\n")
    .split("\n\n")
    .toList
    .map(_.split(List(' ', '\n', ',').toArray).map(_.trim).toList)).get

  val yr = """(\d{4})""".r
  val hgt = """(\d+)(in|cm)""".r
  val hcl = """#[0-9a-f]{6}""".r
  val pid = """\d{9}""".r

  val expectedFields = Map[String, String => Boolean](
    "byr" -> {
      case yr(y) if (1920 to 2002) contains y.toInt => true
      case _ => false
    },
    "iyr" -> {
      case yr(y) if (2010 to 2020) contains y.toInt => true
      case _ => false
    },
    "eyr" -> {
      case yr(y) if (2020 to 2030) contains y.toInt => true
      case _ => false
    },
    "hgt" -> {
      case hgt(h, "cm") => (150 to 193) contains h.toInt
      case hgt(h, "in") => (59 to 76) contains h.toInt
      case _ => false
    },
    "hcl" -> {
      case hcl() => true
      case _ => false
    },
    "ecl" -> { s => Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth") contains s },
    "pid" -> {
      case pid() => true
      case _ => false
    },
  )

  def isValid(fields: List[String])(validField: List[String] => Boolean): Boolean = {
    fields.map(_.split(':').toList).count(validField) == expectedFields.size
  }

  println(passports.count(isValid(_) {
    case field :: _ :: Nil if expectedFields.contains(field) => true
    case _ => false
  }))

  println(passports.count(isValid(_) {
    case field :: value :: Nil if expectedFields.contains(field) && expectedFields(field)(value) => true
    case _ => false
  }))
}
