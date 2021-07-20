package aoc2015

import scala.util.Using

object Day12 extends App {

  import play.api.libs.json._
  import scala.io.Source

  val input = Using(Source.fromFile("inputs/2015/12.txt"))(_.getLines().next()).get

  val json: JsValue = Json.parse(input)

  def addAllInts(json: JsValue, objectFilter: (JsObject) => Boolean, acc: Int = 0): Int = json match {
    case JsNull => acc
    case _: JsBoolean => acc
    case JsNumber(value) => value.intValue + acc
    case JsString(_) => acc
    case JsArray(value) => acc + value.toList.map(x => addAllInts(x, objectFilter)).sum
    case o@JsObject(underlying) => acc + (if (objectFilter(o)) underlying.values.toList.map(x => addAllInts(x, objectFilter)).sum else 0)
  }

  println(addAllInts(json, _ => true))
  println(addAllInts(json, o => o.values.count(_ == JsString("red")) == 0))
}
