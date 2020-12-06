package advent

import scala.io.Source

object DayOne extends App {
  def lines(day: Int): List[String] = Source.fromResource(day.toString).getLines().toList
  lazy val intLines: List[Int] = lines(1).map(_.toInt)

  def resultPartOne(): Int = {
    for {
      x <- intLines
      y <- intLines
      if(x + y) == 2020
    } yield x * y
  }.head

  def resultPartTwo(): Int = {
    for {
      x <- intLines
      y <- intLines
      z <- intLines
      if(x + y + z) == 2020
    } yield x * y * z
  }.head

  println(resultPartOne())
  println(resultPartTwo())
}

