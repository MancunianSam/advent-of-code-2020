package advent

import scala.annotation.tailrec
import scala.io.Source

object DayThree extends App {
  def lines(day: Int): List[String] = Source.fromResource(day.toString).getLines().toList

  def countTrees(right: Int, down: Int): Int = {
    val slope = lines(3)
    val height = slope.length

    def moveRight(x: Int): Int = if (x >= (31 - right)) x + right - 31 else x + right

    @tailrec
    def toboggan(x: Int, y: Int, count: Int): Int = {
      if (y >= height) {
        count
      } else {
        val updatedCount = slope(y).charAt(x) match {
          case '#' => count + 1
          case '.' => count
        }
        toboggan(moveRight(x), y + down, updatedCount)
      }
    }

    toboggan(0, 0, 0)
  }

  def slopesResult(slopes: List[(Int, Int)]) = slopes.map(s => countTrees(s._1, s._2).toLong
  ).foldLeft(1L)((a, b) => a * b)

  def resultPartOne(): Long = {
    slopesResult(List((3,1)))
  }

  def resultPartTwo(): Long = {
    val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    slopesResult(slopes)
  }

  println(resultPartOne())
  println(resultPartTwo())
}
