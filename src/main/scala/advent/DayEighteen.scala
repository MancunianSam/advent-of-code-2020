package advent

import advent.DayEighteen.Precedence.{Addition, Precedence, Same}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex.Groups

object DayEighteen extends App {
  val lines = Source.fromResource("18").getLines.toList
  val bracket = "\\(([\\d\\s*+]*)\\)".r
  val addition = "(\\d*\\s*[+{1}]\\s\\d*)".r

  object Precedence extends Enumeration {
    type Precedence = Value
    val Same, Addition = Value
  }

  def processExpression(expression: String, precedence: Precedence): String =
    precedence match {
      case Same =>
        val strArr = expression.split(" ")
        processExpressionArray(strArr.tail, strArr.head.toLong, precedence)
      case Addition => processExpressionAdditionFirst(expression)
    }

  @tailrec
  def processExpressionArray(expression: Array[String], sum: Long, precedence: Precedence): String = {
    if (expression.isEmpty) {
      sum.toString
    } else {
      val nextNumber = expression(1).toLong
      val nextSum = expression.head match {
        case "*" => sum * nextNumber
        case "+" => sum + nextNumber
      }
      processExpressionArray(expression.splitAt(2)._2, nextSum, precedence)
    }
  }

  @tailrec
  def processExpressionAdditionFirst(expression: String): String = {
    if (!expression.contains("+")) {
      "\\d*".r.findAllIn(expression).toList.filter(!_.isEmpty).map(_.toLong).product.toString
    } else {
      processExpressionAdditionFirst(
        addition.replaceSomeIn(expression, {
          case Groups(l) =>
            val expressionArr = l.split(" ")
            if (expressionArr.size == 3) {
              Try(expressionArr.head.toLong + expressionArr.last.toLong).toOption.map(_.toString)
            } else {
              Option.empty
            }
        })
      )
    }
  }

  @tailrec
  def processLine(line: String, precedence: Precedence): String = {
    if (!line.contains("(")) {
      processExpression(line, precedence)
    } else {
      val newLine = bracket.replaceAllIn(line, _ match {
        case Groups(expression) => processExpression(expression, precedence)
      })
      processLine(newLine, precedence)
    }
  }

  println(lines.map(f => processLine(f, Same)).map(_.toLong).sum)
  println(lines.map(f => processLine(f, Addition)).map(_.toLong).sum)
}
