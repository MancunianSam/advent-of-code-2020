package advent

import scala.annotation.tailrec
import scala.io.Source

object DaySixteen extends App {
  val source = Source.fromResource("16").getLines.toList
  val fields = source.take(source.indexOf(""))
  val yourTicket = source.slice(fields.size + 2, fields.size + 3).head.split(",").toList.map(_.toInt)
  val otherTickets = source.slice(source.indexOf("nearby tickets:") + 1, source.size)

  def range(s: String): Set[Int] = {
    val arr = s.split("-")
    Range.inclusive(arr(0).toInt, arr(1).toInt).toSet
  }

  case class Fields(name: String, range: Set[Int])

  val validNumbersWithName: Set[Fields] = fields.map {
    case s"$name: $firstRange or $secondRange" => Fields(name, range(firstRange) ++ range(secondRange))
  }.toSet

  val validNumberWithNameMap: Map[String, Set[Int]] = validNumbersWithName.map(f => f.name -> f.range).toMap

  val validNumbers: Set[Int] = validNumbersWithName.flatMap(_.range)
  val allTicketFields: List[Int] = otherTickets.flatMap(ticket => ticket.split(",").map(_.toInt))
  val ticketFieldList: List[List[Int]] = otherTickets.map(ticket => ticket.split(",").toList.map(_.toInt))
    .filter(isValid)

  @tailrec
  def isValid(l: List[Int]): Boolean = {
    if (l.isEmpty) {
      true
    } else if (!validNumbers.contains(l.head)) {
      false
    } else {
      isValid(l.tail)
    }
  }

  val numberToNameMap: Map[Int, Set[String]] = allTicketFields.map(f =>
    f -> validNumbersWithName.filter(p => p.range.contains(f)).map(_.name)
  ).toMap

  @tailrec
  def processColumns(idx: Int = 0, foundNames: Map[String, Int] = Map()): Map[String, Int] = {
    if(foundNames.size == ticketFieldList.head.size) {
      foundNames
    } else {
      val columnSet = ticketFieldList.map(f => f(idx)).toSet
      val name = validNumberWithNameMap.filter(f => columnSet.diff(f._2).isEmpty && !foundNames.keySet.contains(f._1))
      val nextIdx = if(idx == ticketFieldList.head.size -1) 0 else idx + 1
      val nextMap = if(name.size == 1) {
        foundNames ++ Map(name.head._1 -> idx)
      } else {
        foundNames
      }
      processColumns(nextIdx, nextMap)
    }
  }

  val columnMap = processColumns()

  val resultPartOne = allTicketFields.filter(f => !validNumbers.contains(f))
  println(resultPartOne.sum)

  val resultPartTwo = columnMap
    .filter(_._1.startsWith("departure"))
    .values.map(idx => yourTicket(idx).toLong)

  println(resultPartTwo.product)

}
