package advent

import scala.io.Source

object DaySix extends App {
  def lines(day: Int): List[String] = Source.fromResource(day.toString).getLines().toList
  val groups: Array[String] = lines(6).mkString("\n").split("\\n\\n")

  def resultPartOne(): Int =
    groups
      .map(gr => gr.replaceAll("\n", "").toCharArray.toSet.size)
      .sum

  def resultPartTwo(): Int =
    groups
      .map(gr =>
        gr.split("\n")
          .map(_.toCharArray.toSet)
      ).map(_
      .reduceLeft((a, b) => {
        a.intersect(b)
      }).size).sum

  println(resultPartOne())
  println(resultPartTwo())
}
