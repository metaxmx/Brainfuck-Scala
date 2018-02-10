package com.illucit.bf

import java.io.IOException

import scala.util.{Failure, Success, Try}

trait Alphabet {

  type Symbol

  val zero: Symbol = numeric.zero

  def numeric: Numeric[Symbol]

  def readSymbol(rw: ReadWriter): IO[Try[Option[Symbol]]]

  def writeSymbol(rw: ReadWriter, symbol: Symbol): IO[Try[Unit]]

}

object Alphabet {

  trait SingleCodePointAlphabet extends Alphabet{

    def fromChar: Char => Symbol

    def toChar: Symbol => Char

    final override def readSymbol(rw: ReadWriter): IO[Try[Option[Symbol]]] = rw.readChar() map { _ map { _ map fromChar} }

    final override def writeSymbol(rw: ReadWriter, symbol: Symbol): IO[Try[Unit]] = rw.writeChar(toChar(symbol))

  }

  object `8-bit` extends SingleCodePointAlphabet {

    override type Symbol = Byte

    override def numeric: Numeric[Byte] = Numeric.ByteIsIntegral

    override def fromChar: Char => Byte = _.toByte

    override def toChar: Byte => Char = _.toChar

  }

  object `16-bit` extends SingleCodePointAlphabet {

    override type Symbol = Char

    override def numeric: Numeric[Char] = Numeric.CharIsIntegral

    override def fromChar: Char => Char = identity[Char]

    override def toChar: Char => Char = identity[Char]

  }

  object `32-bit` extends Alphabet {

    override type Symbol = Int

    override def numeric: Numeric[Int] = Numeric.IntIsIntegral

    final override def readSymbol(rw: ReadWriter): IO[Try[Option[Symbol]]] = rw.readChar() flatMap {
      case Success(Some(highSurrogate)) if highSurrogate.isHighSurrogate =>
        rw.readChar() map {
          _ flatMap {
            case Some(lowSurrogate) if lowSurrogate.isLowSurrogate =>
              Success(Some(Character.toCodePoint(highSurrogate, lowSurrogate)))
            case Some(_) =>
              Failure(new IOException("High-surrogate codepoint not followed by low-surrogate codepoint"))
            case None =>
              Failure(new IOException("High-surrogate codepoint before EOF"))
          }
        }
      case other =>
        IO {
          other map {
            _ map {
              _.toInt
            }
          }
        }
    }

    final override def writeSymbol(rw: ReadWriter, symbol: Symbol): IO[Try[Unit]] = {
      (IO.pure(Success(): Try[Unit]) /: Character.toChars(symbol)) {
        case (io, char) => io flatMap {
          case Success(_) => rw.writeChar(char)
          case failure => IO(failure)
        }
      }
    }

  }


}
