package advent

import scala.annotation.tailrec
import scala.io.Source

object DayFourteen extends App {

  case class Results(arrayIdx: Long, num: Long, mask: String)

  val lines = Source.fromResource("14").getLines.toList

  def getNumber(binary: String, mask: String): String =
    (List.fill(36 - binary.length)(0).mkString ++ binary)
      .lazyZip(mask).map { case (bin, mask) => if (mask == 'X') bin else mask }.mkString

  @tailrec
  def processAddress(locations: List[String]): List[Long] =
    if (!locations.exists(f => f.contains("X"))) {
      locations.map(l => BigInt(l, 2).longValue)
    } else {
      val newLocations = locations.flatMap(binary => {
        val binArr = binary.splitAt(binary.indexOf("X"))
        List("0", "1").map(l => binArr._1 ++ l ++ binArr._2.tail)
      })
      processAddress(newLocations)
    }

  def getMemoryAddresses(binary: String, mask: String): List[Long] =
    processAddress(
      List(
        (List.fill(36 - binary.length)(0).mkString ++ binary).lazyZip(mask).map {
          case (bin, mask) => mask match {
            case '0' => bin
            case '1' => mask
            case 'X' => 'X'
          }
        }))

  lazy val result = lines.foldLeft(List[Results]())((f, a) => {
    val result = a match {
      case s"mask = ${mask}" => Results(0, 0, mask)
      case s"mem[${idx}] = ${value}" =>
        val head = f.head
        val a = getNumber(value.toLong.toBinaryString, head.mask)
        val c = BigInt(a, 2).longValue
        Results(idx.toLong, c, head.mask)
    }
    result :: f.filter(f => f.arrayIdx != result.arrayIdx)
  })

  lazy val resultPartTwo = lines.foldLeft(List[Results]())((f, a) => {
    val results = a match {
      case s"mask = ${mask}" => List(Results(0, 0, mask))
      case s"mem[${idx}] = ${value}" =>
        val head = f.head
        val addresses = getMemoryAddresses(idx.toLong.toBinaryString, head.mask)
        addresses.map(add => Results(add, BigInt(value).longValue, head.mask))
    }
    results ::: f.filter(a => !results.map(_.arrayIdx).contains(a.arrayIdx))
  })

  println(result.map(_.num).sum)
  println(resultPartTwo.map(_.num).sum)
}
