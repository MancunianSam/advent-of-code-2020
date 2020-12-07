package advent

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

object DaySeven extends App {
  val lines: List[String] = Source.fromResource("7").getLines().toList

  case class Bag(count: Int, colour: String)

  val bagMap: Map[String, List[Bag]] =
    lines.map(line => {
    line.split(" contain ") match {
      case Array(containingBag: String, bagsAllowed: String) =>
        val allowedBagsList = bagsAllowed match {
          case "no other bags." => List()
          case _ => bagsAllowed.split(", ").map(bag => {
            val splitBag = bag.split(" ")
            val nameWithoutFullStop = splitBag.tail.mkString(" ").replace(".", "")
            val bagName = nameWithoutFullStop.last match {
              case 'g' => s"${nameWithoutFullStop}s"
              case 's' => nameWithoutFullStop
            }
            Bag(splitBag(0).toInt, bagName)
          }).toList
        }
        containingBag -> allowedBagsList
    }
  }).toMap

  def resultPartOne(): Int = {
    @tailrec
    def checkBag(bags: List[Bag]): List[Int] =
      bags match {
        case Nil => List(0)
        case b: List[Bag] =>
          if (b.exists(_.colour.contains("shiny gold bag"))) {
            List(1)
          } else {
            val subBags: List[Bag] = b.flatMap(c => bagMap(c.colour))
            checkBag(subBags)
          }
      }

    bagMap.flatMap { case (containingBag, bagsAllowed) =>
      checkBag(bagsAllowed)
    }
  }.sum

  def resultPartTwo(): Int = {

    def countBag(bags: List[Bag]): List[Bag] =
      bags match {
        case Nil => bags
        case b: List[Bag] =>
          val subBags = for {
            c <- bags
            _ <- (1 to c.count).toList
            d <- bagMap(c.colour)
          } yield d
          bags ::: countBag(subBags)
      }
    countBag(bagMap("shiny gold bags")).map(_.count).sum
  }

  println(resultPartTwo())
}
