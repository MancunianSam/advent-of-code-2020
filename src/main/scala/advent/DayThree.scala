package advent

import Loader._

import scala.annotation.tailrec

object DayThree {

  def countTrees(right: Int, down: Int): Int = {
    val slope = lines(3)
    val height = slope.length

    def moveRight(x: Int): Int = if (x >= (31 - right)) x + right - 31 else x + right

    @tailrec
    def toboggan(x: Int, y: Int, count: Int): Int = {
      if (y >= height) {
        println(count)
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

  def resultPartOne(): Long = {
    val slopes = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    slopes.map(s => countTrees(s._1, s._2).toLong
    ).foldLeft(1L)((a, b) => a * b)
  }
}
