package advent

import scala.annotation.tailrec
import scala.io.Source

object DayNine extends App {
  val lines = Source.fromResource("9").getLines.toList.map(_.toLong)

  def calculateSums(l: List[Long]): Set[Long] = {
    for {
      l1 <- l
      l2 <- l
    } yield l1 + l2
  }.toSet

  val (preamble, rest) = lines.splitAt(25)

  @tailrec
  def checkValue(preamble: List[Long], pos: Int): Long = {
    val value = rest(pos)
    if (!calculateSums(preamble).contains(value)) {
      value
    } else {
      checkValue(preamble.tail ++ List(value), pos + 1)
    }
  }

  val resultPartOne = checkValue(preamble, 0)
  println(resultPartOne)

  @tailrec
  def checkRange(list: List[Long]): Long = {
    val range = (1 to list.size).takeWhile(r => list.take(r).sum != resultPartOne)
    if (range.size < list.length) {
      val sorted = list.take(range.last + 1).sorted
      sorted.min + sorted.max
    } else {
      checkRange(list.tail)
    }
  }

  println(checkRange(lines))
}
