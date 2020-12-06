package advent

import advent.DaySix.{resultPartOne, resultPartTwo}

import scala.io.Source

object DayFour extends App {

  def lines(day: Int): List[String] = Source.fromResource(day.toString).getLines().toList
  case class Passport(byr: Option[String], iyr: Option[String], eyr: Option[String], hgt: Option[String], hcl: Option[String], ecl: Option[String], pid: Option[String])

  def heightFilter(height: String): Boolean = {
    val num = height.split("cm|in").head.toInt
    height.split("\\d").lastOption match {
      case Some("cm") => 150 to 193 contains num
      case Some("in") => 59 to 76 contains num
      case None => false
    }
  }

  val eyeColours = List("amb", "blu", "brn", "gry", "grn", "hzl", "oth")

  implicit class PassportMapUtils(pm: Map[String, String]) {
    def toPassport: _root_.advent.DayFour.Passport = Passport(pm.get("byr"), pm.get("iyr"), pm.get("eyr"), pm.get("hgt"), pm.get("hcl"), pm.get("ecl"), pm.get("pid"))
  }

  def isComplete(p: Passport): Boolean = p.byr.isDefined && p.iyr.isDefined && p.eyr.isDefined && p.hgt.isDefined && p.hcl.isDefined & p.ecl.isDefined && p.pid.isDefined

  def isCompletePartTwo(p: Passport): Boolean = {
    for {
      byr <- p.byr if 1920 to 2002 contains byr.toInt
      iyr <- p.iyr if 2010 to 2020 contains iyr.toInt
      eyr <- p.eyr if 2020 to 2030 contains eyr.toInt
      hgt <- p.hgt if heightFilter(hgt)
      hcl <- p.hcl
      col <- "^#[0-9a-z]{6}".r.findFirstMatchIn(hcl) if col.toString == hcl
      ecl <- p.ecl if eyeColours.contains(ecl)
      pid <- p.pid
      pidid <- "[0-9]{9}".r.findFirstMatchIn(pid) if pidid.toString == pid
    } yield pid
  }.isDefined

  def result(fn: Passport => Boolean): Int = {
    val s = lines(4)
    s.mkString("\n").split("\\n\\n")
      .count(f => {
        fn(f.split("\\s|\\n").map(_.split(":"))
          .map(f => f(0) -> f(1)).toMap.toPassport)
      })
  }

  println(result(isComplete))
  println(result(isCompletePartTwo))
}
