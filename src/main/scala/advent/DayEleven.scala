package advent

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object DayEleven extends App {

  val seating = Source.fromResource("11").getLines.toList.map(_.toCharArray.toList)

  @tailrec
  def runRoundsPartTwo(seats: List[List[Char]], count: Int): Int = {

    @tailrec
    def isFirstSeatOccupied(columnIdx: Int, rowIdx: Int, colFn: Int => Int, rowFn: Int => Int, path: List[Char] = Nil): Boolean = {
      val seatsTry = Try(seats(colFn(columnIdx))(rowFn(rowIdx)))
      if (seatsTry.isFailure) {
        path.headOption.getOrElse('.') == '#'
      } else if (seatsTry.get == '#' || seatsTry.get == 'L') {
        seatsTry.get == '#'
      } else {
        isFirstSeatOccupied(colFn(columnIdx), rowFn(rowIdx), colFn, rowFn, seatsTry.get :: path)
      }
    }

    def countAdjacentOccupied(columnIdx: Int, rowIdx: Int) = {
      Seq(
        isFirstSeatOccupied(columnIdx, rowIdx, c => c, r => r - 1), //left
        isFirstSeatOccupied(columnIdx, rowIdx, c => c, r => r + 1), //right
        isFirstSeatOccupied(columnIdx, rowIdx, c => c + 1, r => r), //down
        isFirstSeatOccupied(columnIdx, rowIdx, c => c - 1, r => r), //up
        isFirstSeatOccupied(columnIdx, rowIdx, c => c + 1, r => r - 1), //downLeft
        isFirstSeatOccupied(columnIdx, rowIdx, c => c + 1, r => r + 1), //downRight
        isFirstSeatOccupied(columnIdx, rowIdx, c => c - 1, r => r - 1), //upLeft
        isFirstSeatOccupied(columnIdx, rowIdx, c => c - 1, r => r + 1) //upRight
      ).count(p => p)
    }

    def nextRound(): List[List[Char]] = {
      seats.zipWithIndex map {
        case (row, columnIdx) => row.zipWithIndex map {
          case (rowElement, rowIdx) =>
            val occupied = countAdjacentOccupied(columnIdx, rowIdx)
            rowElement match {
              case 'L' if occupied == 0 => '#'
              case '#' if occupied >= 5 => 'L'
              case _ => rowElement
            }
        }
      }
    }

    val newCount = seats.flatten.count(_ == '#')
    if (newCount == count && newCount > 0) {
      count
    } else {
      val next = nextRound()
      next.foreach(r => println(r.mkString))
      println()
      runRoundsPartTwo(next, newCount)
    }
  }

  @tailrec
  def runRounds(seats: List[List[Char]], count: Int): Int = {
    def isOccupied(column: Int, row: Int) = {
      Try(seats(column)(row)).map(f => f == '#')
    }

    def countAdjacentOccupied(columnIdx: Int, rowIdx: Int) = {
      Seq(
        isOccupied(columnIdx, rowIdx - 1), // left
        isOccupied(columnIdx, rowIdx + 1), // right
        isOccupied(columnIdx - 1, rowIdx), // down
        isOccupied(columnIdx + 1, rowIdx), // up
        isOccupied(columnIdx + 1, rowIdx - 1), // up left
        isOccupied(columnIdx - 1, rowIdx + 1), // down right
        isOccupied(columnIdx - 1, rowIdx - 1), // down left
        isOccupied(columnIdx + 1, rowIdx + 1) // up right
      ).count(i => i.isSuccess && i.get)
    }

    def nextRound(): List[List[Char]] = {
      seats.zipWithIndex map {
        case (row, columnIdx) => row.zipWithIndex map {
          case (rowElement, rowIdx) =>
            val occupied = countAdjacentOccupied(columnIdx, rowIdx)
            rowElement match {
              case 'L' if occupied == 0 => '#'
              case '#' if occupied >= 4 => 'L'
              case _ => rowElement
            }
        }
      }
    }

    val newCount = seats.flatten.count(_ == '#')
    if (newCount == count && newCount > 0) {
      count
    } else {
      runRounds(nextRound(), newCount)
    }
  }

  println(runRounds(seating, 0))
  println(runRoundsPartTwo(seating, 0))

}
