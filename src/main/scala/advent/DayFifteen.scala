package advent

import scala.annotation.tailrec

object DayFifteen extends App {

  @tailrec
  def calculate(map: Map[Int, List[Int]], num: Int, finalIdx: Int, idx: Int): Int = {
    if (idx == finalIdx + 1) {
      num
    } else {
      val indexes = map.getOrElse(num, List())
      val newMap = {
        val newNum = indexes.head - indexes.lastOption.getOrElse(0)
        newNum -> map.get(newNum).map(l => List(idx,l.head)).getOrElse(List(idx))
      }
      calculate(map + newMap, newMap._1, finalIdx, idx + 1)
    }
  }

  val m = List(10, 16, 6, 0, 1, 17).zipWithIndex.map { case (a, b) => a -> List(b + 1) }.toMap

  println(calculate(m, m.last._1, 2020, m.size + 1))
  println(calculate(m, m.last._1, 30000000, m.size + 1))
}
