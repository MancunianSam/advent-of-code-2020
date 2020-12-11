package advent

import scala.annotation.tailrec
import scala.io.Source

object DayTen extends App {

  val lines = Source.fromResource("10").getLines.toList.map(_.toInt).sorted

  @tailrec
  def findDiff(l: List[Int], idx: Int = 0, diffs: List[Int] = Nil): List[Int] = {
    if (idx == l.length - 1) {
      diffs
    } else {
      findDiff(l, idx + 1, l(idx + 1) - l(idx) :: diffs)
    }
  }

  def diffs(currentJolt: Int, jolts: List[Int]): List[Int] = {
    val allJolts = 0 :: nextJolts(currentJolt, jolts)
    findDiff(allJolts)
  }

  def nextJolts(currentJolt: Int, jolts: List[Int], outputJolts: List[Int] = Nil): List[Int] = {
    val range: List[Int] = List.range(currentJolt + 1, currentJolt + 4)
    val nextJolt = jolts.find(p => range.contains(p)).getOrElse(lines.max + 3)
    jolts.partition(_ <= currentJolt)._2 match {
      case Nil =>
        outputJolts :+ nextJolt
      case l: List[Int] => nextJolts(nextJolt, l, outputJolts :+ nextJolt)
    }
  }

  def calculateNext(current: Int): List[Int] = lines.filter(List.range(current + 1, current + 4).contains)

  val diff = diffs(0, lines)
  println(diff.count(_ == 1) * diff.count(_ == 3))

  val idxOfThree = diff.zipWithIndex.filter(pair => pair._1 == 3).map(pair => pair._2) :+ (diff.length)

  @tailrec
  def diffBetweenIdxOfThree(idx: Int, l: List[Int]):List[Int] = {
    if(idx + 1 >= idxOfThree.size) {
      l
    } else {
      val newL = idxOfThree(idx+1) - idxOfThree(idx) - 1  :: l
      diffBetweenIdxOfThree(idx + 1, newL)
    }
  }
  println(idxOfThree)
  val diffBetween = diffBetweenIdxOfThree(0, List()).filter(_ > 1)
  val x = diffBetween.count(_ == 2)
  val y = diffBetween.count(_ == 3)
  val z = diffBetween.count(_ == 4)

  val result = scala.math.pow(2, x).toLong * scala.math.pow(4, y).toLong * scala.math.pow(7, z).toLong
  println(result)




}
