package advent

import scala.annotation.tailrec
import scala.io.Source

object DayTwelve extends App {

  trait Instruction {
    def fn: Position => Position
  }

  trait Position {
    def turnLeft(degrees: Int): Position
    def turnRight(degrees: Int): Position
    def moveNorth(distance: Int): Position
    def moveSouth(distance: Int): Position
    def moveEast(distance: Int): Position
    def moveWest(distance: Int): Position
    def moveForward(distance: Int): Position
    def absolute: Int
  }

  val directions = List("North", "East", "South", "West", "North", "East", "South", "West")

  def left(degrees: Int, direction: String): String = directions(directions.lastIndexOf(direction) - (degrees / 90))

  def right(degrees: Int, direction: String): String = directions(directions.indexOf(direction) + (degrees / 90))

  case class Forward(fn: Position => Position) extends Instruction
  case class Left(fn: Position => Position) extends Instruction
  case class Right(fn: Position => Position) extends Instruction
  case class North(fn: Position => Position) extends Instruction
  case class South(fn: Position => Position) extends Instruction
  case class East(fn: Position => Position) extends Instruction 
  case class West(fn: Position => Position) extends Instruction

  case class PositionPartOne(north: Int, south: Int, east: Int, west: Int, direction: String) extends Position {
    def turnLeft(degrees: Int): PositionPartOne = PositionPartOne(this.north, this.south, this.east, this.west, left(degrees, this.direction))
    def turnRight(degrees: Int): Position = PositionPartOne(this.north, this.south, this.east, this.west, right(degrees, this.direction))
    def moveNorth(distance: Int): PositionPartOne = PositionPartOne(math.max(0, this.north + distance - this.south), math.max(0, this.south - distance), this.east, this.west, this.direction)
    def moveSouth(distance: Int): PositionPartOne = PositionPartOne(math.max(0, this.north - distance), math.max(0, this.south + distance - this.north), this.east, this.west, this.direction)
    def moveEast(distance: Int): PositionPartOne = PositionPartOne(this.north, this.south, math.max(0, this.east + distance - this.west), math.max(0, this.west - distance), this.direction)
    def moveWest(distance: Int): PositionPartOne = PositionPartOne(this.north, this.south, math.max(0, this.east - distance), math.max(0, this.west + distance - this.east), this.direction)
    def moveForward(distance: Int): PositionPartOne = this.direction match {
      case "North" => this.moveNorth(distance)
      case "South" => this.moveSouth(distance)
      case "East" => this.moveEast(distance)
      case "West" => this.moveWest(distance)
    }
    def absolute: Int = math.abs(this.north - this.south) + math.abs(this.east - this.west)
  }

  case class Waypoint(north: Int, south: Int, east: Int, west: Int)

  case class PositionPartTwo(north: Int, south: Int, east: Int, west: Int, waypoint: Waypoint) extends Position {
    def getDirectionWaypointValue(direction: String) = direction match {
      case "North" => this.waypoint.north
      case "South" => this.waypoint.south
      case "East" => this.waypoint.east
      case "West" => this.waypoint.west
    }
    def turn(degrees: Int, fn: (Int, String) => String): PositionPartTwo = {
      val west = getDirectionWaypointValue(fn(degrees, "West"))
      val south = getDirectionWaypointValue(fn(degrees, "South"))
      val east = getDirectionWaypointValue(fn(degrees, "East"))
      val north = getDirectionWaypointValue(fn(degrees, "North"))
      PositionPartTwo(this.north, this.south, this.east, this.west, Waypoint(north, south, east, west))
    }

    override def turnLeft(degrees: Int): Position = turn(degrees, right)
    override def turnRight(degrees: Int): Position = turn(degrees, left)
    def moveNorth(distance: Int): PositionPartTwo = PositionPartTwo(this.north, this.south, this.east, this.west, Waypoint(math.max(0, this.waypoint.north + distance - this.waypoint.south), math.max(0, this.waypoint.south - distance), this.waypoint.east, this.waypoint.west))
    def moveSouth(distance: Int): PositionPartTwo = PositionPartTwo(this.north, this.south, this.east, this.west, Waypoint(math.max(0, this.waypoint.north - distance), math.max(0, this.waypoint.south + distance - this.waypoint.north), this.waypoint.east, this.waypoint.west))
    def moveEast(distance: Int): PositionPartTwo = PositionPartTwo(this.north, this.south, this.east, this.west, Waypoint(this.waypoint.north, this.waypoint.south, math.max(0, this.waypoint.east + distance - this.waypoint.west), math.max(0, this.waypoint.west - distance)))
    def moveWest(distance: Int): PositionPartTwo = PositionPartTwo(this.north, this.south, this.east, this.west, Waypoint(this.waypoint.north, this.waypoint.south, math.max(0, this.waypoint.east - distance), math.max(0, this.waypoint.west + distance - this.waypoint.east)))
    override def absolute: Int = math.abs(this.north - this.south) + math.abs(this.east - this.west)

    override def moveForward(distance: Int): Position = {
      val north = math.max(0, this.north + (this.waypoint.north * distance) - (this.south + (this.waypoint.south * distance)))
      val south = math.max(0, this.south + (this.waypoint.south * distance) - (this.north + (this.waypoint.north * distance)))
      val east = math.max(0, this.east + (this.waypoint.east * distance) - (this.west + (this.waypoint.west * distance)))
      val west = math.max(0, this.west + (this.waypoint.west * distance) - (this.east + (this.waypoint.east * distance)))
      PositionPartTwo(north, south, east, west, this.waypoint.copy())
    }
  }

  val lines: List[Instruction] = Source.fromResource("12").getLines.toList.map(f => {
    val (instruction, modifier) = f.splitAt(1)
    instruction match {
      case "F" => Forward(_.moveForward(modifier.toInt))
      case "L" => Left(_.turnLeft(modifier.toInt))
      case "R" => Right(_.turnRight(modifier.toInt))
      case "N" => North(_.moveNorth(modifier.toInt))
      case "S" => South(_.moveSouth(modifier.toInt))
      case "E" => East(_.moveEast(modifier.toInt))
      case "W" => West(_.moveWest(modifier.toInt))
    }
  })

  @tailrec
  def calculateNextPosition(idx: Int, position: Position): Position = {
    if (idx == lines.length) {
      position
    } else {
      calculateNextPosition(idx + 1, lines(idx).fn(position))
    }
  }

  println(calculateNextPosition(0, PositionPartOne(0, 0, 0, 0, "East")).absolute)
  println(calculateNextPosition(0, PositionPartTwo(0,0,0,0,Waypoint(1, 0, 10, 0))).absolute)


}
