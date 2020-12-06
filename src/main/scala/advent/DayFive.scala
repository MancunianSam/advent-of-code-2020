package advent



import advent.DaySix.{resultPartOne, resultPartTwo}

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

object DayFive extends App {
  def lines(day: Int): List[String] = Source.fromResource(day.toString).getLines().toList

  implicit class CharUtils(c: Char) {
    def idx: Int = c match {
      case 'F' => 0
      case 'B' => 1
      case 'R' => 1
      case 'L' => 0
    }
  }

  def result(str: String, range: Range.Inclusive): Int = {
    @tailrec
    def process(ints: List[Int], pos: Int): Int =
      ints.size match {
        case 1 => ints.head
        case _ =>
          val groupedInts = ints.grouped(ints.size / 2).toList
          process(groupedInts(str.charAt(pos).idx), pos + 1)
      }

    process(range.toList, 0)
  }

  val seatNumbers: List[Int] = lines(5).map(str => {
    val rowString = str.substring(0, 7)
    val columnString = str.substring(7, 10)
    val seatRow = result(rowString, 0 to 127)
    val seatColumn = result(columnString, 0 to 7)
    seatRow * 8 + seatColumn
  })

  def resultPartTwo(): Option[Int] = {
    val allSeats = for {
      row <- (0 to 127).toList
      column <- (0 to 7).toList
    } yield row * 8 + column
    allSeats.toSet.diff(seatNumbers.toSet).find(num => seatNumbers.contains(num + 1) && seatNumbers.contains(num -1))
  }

  println(seatNumbers.max)
  println(resultPartTwo())
}
