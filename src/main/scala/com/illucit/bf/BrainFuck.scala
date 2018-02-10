package com.illucit.bf

import com.illucit.bf.BrainFuck.RuntimeConstraints

import scala.util.{Failure, Success}

/**
  * BrainFuck interpreter
  */
class BrainFuck(val alphabet: Alphabet, readWriter: ReadWriter, constraints: RuntimeConstraints) {

  import BrainFuck._
  import alphabet._

  implicit private val numeric: Numeric[Symbol] = alphabet.numeric
  import numeric.{minus, one, plus}

  case class Machine(leftStack: List[Symbol] = Nil, current: Symbol = zero, rightStack: List[Symbol] = Nil) {
    def size: Int = leftStack.size + rightStack.size + 1
    def + : Machine = copy(current = plus(current, one))
    def - : Machine = copy(current = minus(current, one))
    def < : Machine = leftStack match {
      case Nil => Machine(Nil, zero, current :: rightStack)
      case left :: leftTail => Machine(leftTail, left, current :: rightStack)
    }
    def > : Machine = rightStack match {
      case Nil => Machine(current :: leftStack, zero, Nil)
      case right :: rightTail => Machine(current :: leftStack, right, rightTail)
    }
    def `.`: Symbol = current
    def `,`(c: Symbol): Machine = copy(current = c)
    override def toString: String = printMachine(this)
  }

  def initialMachine: Machine = Machine()

  case class Code private[BrainFuck] (code: IndexedSeq[Char], pointer: Int = 0, loopStack: List[Int] = Nil, skipLoop: Option[Int] = None, ticks: Int = 0) extends Execution {
    def currentChar: Char = code(pointer)
    def terminated: Boolean = pointer >= code.length
    def `[`(current: Symbol): Code = current match {
      case `zero` => copy(loopStack = pointer :: loopStack, skipLoop = skipLoop orElse Some(pointer))
      case _ => copy(loopStack = pointer :: loopStack)
    }
    def `]`(current: Symbol): Either[Execution, Code] = loopStack match {
      case loopBegin :: restLoopStack if skipLoop contains loopBegin => Right(copy(loopStack = restLoopStack, skipLoop = None))
      case _ :: restLoopStack if current == zero || skipLoop.isDefined => Right(copy(loopStack = restLoopStack))
      case loopBegin :: _ => Right(copy(pointer = loopBegin))
      case Nil => Left(UnterminatedLoop)
    }
    def next: Execution = if(pointer >= code.length - 1) Terminated else copy(pointer = pointer + 1, ticks = ticks + 1)
    override def toString: String = printCode(this)
  }

  case class ProgramState(execution: Execution, machine: Machine)

  def execute(program: String): IO[FinalExecution] = {
    val initialExecution = program.toCharArray.filter(PROGRAM_CHARS.contains) match {
      case Array.emptyCharArray => Terminated
      case code => Code(code.toVector)
    }
    val initialState = ProgramState(initialExecution, Machine())
    IO pure { initialState } flatMap { execute }
  }

  private def execute(state: ProgramState): IO[FinalExecution] = {
    val execIO = (state.execution, state.machine) match {
      case (finalExec: FinalExecution, _) => IO(finalExec)
      case (code: Code, _) if constraints.maxTime.exists(code.ticks > _) => IO(TimeLimitExceeded)
      case (_, machine) if constraints.maxSize.exists(machine.size > _) => IO(SpaceLimitExceeded)
      case (code: Code, machine: Machine) =>
        code.currentChar match {
          case '[' =>
            IO { ProgramState(
              code.`[`(machine.current).next, machine
            ) } flatMap execute
          case ']' =>
            IO { ProgramState(
              code.`]`(machine.current).fold(identity, _.next), machine
            ) } flatMap execute
          case _ if code.skipLoop.isDefined =>
            IO { ProgramState(
              code.next, machine
            ) } flatMap execute
          case '+' =>
            IO { ProgramState(
              code.next, machine.+
            ) } flatMap execute
          case '-' =>
            IO { ProgramState(
              code.next, machine.-
            ) } flatMap execute
          case '<' =>
            IO { ProgramState(
              code.next, machine.<
            ) } flatMap execute
          case '>' =>
            IO { ProgramState(
              code.next, machine.>
            ) } flatMap execute

          case ',' =>
            alphabet.readSymbol(readWriter) flatMap {
              case Failure(e) =>
                IO(IOError(e))
              case Success(None) =>
                IO(ReadFromEOF)
              case Success(Some(symbol)) =>
                IO { ProgramState(
                  code.next, machine.`,`(symbol)
                ) } flatMap execute
            }
          case '.' =>
            alphabet.writeSymbol(readWriter, machine.`.`) map {
              _ => ProgramState(
                code.next, machine
              )
            } flatMap execute
        }
    }
    readWriter match {
      case debugger: DebugWriter =>
        for {
          _ <- debugger.writeDebugLine(state.machine.toString)
          _ <- debugger.writeDebugLine(state.execution.toString)
          _ <- debugger.writeDebugLine("")
          result <- execIO
        } yield result
      case _  =>
        execIO
    }
  }

  def printMachine(machine: Machine): String = {
    import Console.{BLUE, RESET, WHITE}
    machine.leftStack.reverse.map("[" + _.toString.format("%3s") + "]").mkString(WHITE + "... " + RESET, "  ", " ") +
      BLUE + "<[" + machine.current.toString.format("%3s") + "]>" + RESET +
      machine.rightStack.map("[" + _.toString.format("%1$3s") + "]").mkString(" ", "  ", WHITE + " ..." + RESET)
  }

  def printCode(code: Code): String = {
    import Console.{BLUE, RESET, YELLOW, YELLOW_B}
    val codeFull = code.code.zipWithIndex.map {
      case (c, code.pointer) => YELLOW_B + BLUE + "(" + c + ")" + RESET
      case (c, _) => " " + c + " "
    }
    codeFull.grouped(50).map(YELLOW + ">> " + RESET + _.mkString).mkString("\n")
  }

}

object BrainFuck {

  val PROGRAM_CHARS: Set[Char] = Set('.', ',', '<', '>', '[', ']', '+', '-')

  sealed trait Execution

  sealed trait FinalExecution extends Execution

  case object Terminated extends FinalExecution

  trait Error extends FinalExecution

  case object UnterminatedLoop extends Error

  case object ReadFromEOF extends Error

  case class IOError(cause: Throwable) extends Error

  case object TimeLimitExceeded extends Error

  case object SpaceLimitExceeded extends Error

  case class RuntimeConstraints(maxSize: Option[Int], maxTime: Option[Int])

  object RuntimeConstraints {

    def empty: RuntimeConstraints = RuntimeConstraints(None, None)

  }

  def apply(alphabet: Alphabet = Alphabet.`8-bit`, readWriter: ReadWriter = ConsoleReadWriter, constraints: RuntimeConstraints = RuntimeConstraints.empty): BrainFuck =
    new BrainFuck(alphabet, readWriter, constraints)

}
