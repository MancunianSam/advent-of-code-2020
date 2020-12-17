package advent

import scala.annotation.tailrec
import scala.io.Source

object DaySeventeen extends App {

  trait State {}
  trait Cube extends Product with Serializable {
    def neighbours(): List[Cube]
    def currentState(stateMap: Map[Cube, State]): State = stateMap.getOrElse(this, Inactive())
    def nextState(stateMap: Map[Cube, State]): State = {
      val neighbours = this.neighbours()
      val active = neighbours.map(_.currentState(stateMap)) count  {
        case Active() => true
        case _ => false
      }

      val a = this.currentState(stateMap) match {
        case Active() => if (List(2, 3).contains(active)) Active() else Inactive()
        case Inactive() => if (active == 3) Active() else Inactive()
        case _ => Inactive()
      }
      a
    }
  }

  case class Active() extends State

  case class Inactive() extends State

  case class StateCount(active: Long, inactive: Long)

  val source = Source.fromResource("17").getLines.toList.map(_.toCharArray.toList)
  val inital3DUniverse: List[List[List[Char]]] = List(source)
  val inital4DUniverse: List[List[List[List[Char]]]] = List(List(source))

  lazy val hyperCubeState: Map[Cube, State] = {
    for {
      w <- inital4DUniverse.zipWithIndex
      z <- w._1.zipWithIndex
      y <- z._1.zipWithIndex
      x <- y._1.zipWithIndex
    } yield HyperCube(x._2, y._2, z._2, w._2) -> (inital4DUniverse(w._2)(z._2)(y._2)(x._2) match {
      case '#' => Active()
      case '.' => Inactive()
    })
  }.toMap

  lazy val normalCubeState: Map[Cube, State] = {
    for {
      z <- inital3DUniverse.zipWithIndex
      y <- z._1.zipWithIndex
      x <- y._1.zipWithIndex
    } yield NormalCube(x._2, y._2, z._2) -> (inital3DUniverse(z._2)(y._2)(x._2) match {
      case '#' => Active()
      case '.' => Inactive()
    })
  }.toMap

  case class HyperCube(x: Long, y: Long, z: Long, w: Long) extends Cube {
    override def neighbours(): List[Cube] = for {
      x <- List(this.x - 1, this.x, this.x + 1)
      y <- List(this.y - 1, this.y, this.y + 1)
      z <- List(this.z - 1, this.z, this.z + 1)
      w <- List(this.w - 1, this.w, this.w + 1)
      if (x, y, z, w) != (this.x, this.y, this.z, this.w)
    } yield HyperCube(x, y, z, w)

  }

  case class NormalCube(x: Long, y: Long, z: Long) extends Cube {
    def neighbours(): List[NormalCube] = for {
      x <- List(this.x - 1, this.x, this.x + 1)
      y <- List(this.y - 1, this.y, this.y + 1)
      z <- List(this.z - 1, this.z, this.z + 1)
      if (x, y, z) != (this.x, this.y, this.z)
    } yield NormalCube(x, y, z)
  }

  @tailrec
  def runCycles(stateMap: Map[Cube, State], cycle: Long = 0, count: Long = 0): Long = {
    if (cycle == 6) {
      count
    } else {
      val allPoints = stateMap.keySet
      val newState = allPoints
        .flatMap(_.neighbours())
        .map(eachCube =>
          eachCube -> eachCube.nextState(stateMap)).toMap
      val newCount = newState.count(c => c._2 match {
        case Active() => true
        case Inactive() => false
        case _ =>false
      })
      runCycles(newState, cycle + 1, newCount)
    }
  }

  println(runCycles(normalCubeState))
  println(runCycles(hyperCubeState))
}
