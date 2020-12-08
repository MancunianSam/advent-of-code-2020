package advent

import scala.annotation.tailrec
import scala.io.Source

object DayEight extends App {

  trait Operation

  case class Nop() extends Operation

  case class Acc() extends Operation

  case class Jmp() extends Operation

  case class Instruction(operation: Operation, argument: Int, idx: Int)

  val lines = Source.fromResource("8").getLines().toList

  val instructions: List[Instruction] = lines.zipWithIndex.map {
    case (line, idx) =>
      "(acc|jmp|nop)\\s([-+]\\d*)".r.findAllMatchIn(line).toList match {
        case ::(head, _) =>
          val operation = head.group(1) match {
            case "nop" => Nop()
            case "acc" => Acc()
            case "jmp" => Jmp()
          }
          Instruction(operation, head.group(2).toInt, idx)
      }
  }

  @tailrec
  def processInstructions(acc: Int, instructionPosition: Int, executedPositions: List[Int], instructions: List[Instruction]): (Int, Int) = {
    if (executedPositions.contains(instructionPosition) || instructionPosition == instructions.length) {
      (acc, instructionPosition)
    } else {
      val instruction = instructions(instructionPosition)
      val args: (Int, Int) = instruction.operation match {
        case Acc() => (acc + instruction.argument, instructionPosition + 1)
        case Jmp() => (acc, instructionPosition + instruction.argument)
        case Nop() => (acc, instructionPosition + 1)
      }
      processInstructions(args._1, args._2, instructionPosition :: executedPositions, instructions)
    }
  }

  @tailrec
  def processBrokenSet(currentPosition: Int): Int = {
    val i: Instruction = instructions(currentPosition)
    val newInstructions: List[Instruction] = instructions.patch(currentPosition, List(Instruction(swapOp(i.operation), i.argument, i.idx)), 1)
    val (acc, pos) = processInstructions(0, 0, List(), newInstructions)
    if (pos == instructions.length) {
      acc
    } else {
      processBrokenSet(findNextInstructionPosition(currentPosition))
    }
  }

  def swapOp(operation: Operation): Operation =
    operation match {
      case n: Nop => Jmp()
      case j: Jmp => Nop()
    }

  def isNopOrJmp(operation: Operation): Boolean =
    operation match {
      case n: Nop => true
      case j: Jmp => true
      case _ => false
    }

  def findNextInstructionPosition(currentPosition: Int) =
    instructions.find(i => isNopOrJmp(i.operation) && i.idx > currentPosition).map(_.idx).get

  println(processInstructions(0, 0, List(), instructions))
  println(processBrokenSet(findNextInstructionPosition(0)))
}
