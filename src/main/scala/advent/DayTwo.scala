package advent

import scala.io.Source

object DayTwo extends App {
  def lines(day: Int): List[String] = Source.fromResource(day.toString).getLines().toList

  case class Passwords(range: Range, letter: Char, password: String)
  case class PasswordsPartTwo(positions: (Int, Int), letter: Char, password: String)

  def resultPartOne(): Int = lines(2).map(line => {
    line.replace(":", "").split(" ") match {
      case Array(rangeString, letter, password) =>
        val range = rangeString.split("-") match {
          case Array(first, second) => first.toInt to second.toInt
        }
        Passwords(range, letter.head, password)
    }
  }).count(p => p.range.contains(p.password.count(_ == p.letter)))

  def resultPartTwo(): Int = lines(2).map(line => {
    line.replace(":", "").split(" ") match {
      case Array(rangeString, letter, password) =>
        val range = rangeString.split("-") match {
          case Array(first, second) => (first.toInt, second.toInt)
        }
        PasswordsPartTwo(range, letter.head, password)
    }
  }).count(p => p.password.charAt(p.positions._1 - 1) == p.letter ^ p.password.charAt(p.positions._2 - 1) == p.letter)

  println(resultPartOne())
  println(resultPartTwo())
}
