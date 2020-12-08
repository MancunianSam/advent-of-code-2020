package advent

import scala.annotation.tailrec
import scala.io.Source

object DayEight extends App {
  case class Input(acc: Int, pos: Int, arg: Int)

  case class Output(acc: Int, pos: Int)

  trait Operation {
    def next(vals: Input): Output
  }

  case class Nop() extends Operation {
    override def next(vals: Input): Output = Output(vals.acc, vals.pos + 1)
  }

  case class Acc() extends Operation {
    override def next(vals: Input): Output = Output(vals.acc + vals.arg, vals.pos + 1)
  }

  case class Jmp() extends Operation {
    override def next(vals: Input): Output = Output(vals.acc, vals.pos + vals.arg)
  }

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
      val out = instruction.operation.next(Input(acc, instructionPosition, instruction.argument))
      processInstructions(out.acc, out.pos, instructionPosition :: executedPositions, instructions)
    }
  }

  @tailrec
  def processBrokenSet(currentPosition: Int): Int = {
    val i: Instruction = instructions(currentPosition)
    val newInstructions: List[Instruction] = instructions.patch(currentPosition, List(Instruction(i.operation.swapOp, i.argument, i.idx)), 1)
    val (acc, pos) = processInstructions(0, 0, List(), newInstructions)
    if (pos == instructions.length) {
      acc
    } else {
      processBrokenSet(findNextInstructionPosition(currentPosition))
    }
  }

  implicit class OperationUtils(operation: Operation) {
    def swapOp: Operation = operation match {
      case n: Nop => Jmp()
      case j: Jmp => Nop()
    }

    def isNopOrJmp: Boolean =
      operation match {
        case Nop() | Jmp() => true
        case _ => false
      }
  }

  def findNextInstructionPosition(currentPosition: Int): Int =
    instructions.find(i => i.operation.isNopOrJmp && i.idx > currentPosition).map(_.idx).get

  println(processInstructions(0, 0, List(), instructions))
  println(processBrokenSet(findNextInstructionPosition(0)))
}
