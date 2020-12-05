package advent

import java.io.File

import scala.io.Source

object Loader {
  def lines(day: Int): List[String] = {
    val source = Source.fromFile(new File(s"/home/sam/utils/advent-of-code/$day"))
    source.getLines().toList
  }

  def lineString(day: Int): String = {
    val source = Source.fromFile(new File(s"/home/sam/utils/advent-of-code/$day"))
    source.getLines().mkString
  }
}
