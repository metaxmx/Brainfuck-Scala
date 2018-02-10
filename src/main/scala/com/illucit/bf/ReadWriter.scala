package com.illucit.bf

import scala.io.{BufferedSource, Source}
import scala.util.Try

trait ReadWriter {

  def readChar(): IO[Try[Option[Char]]]

  def writeChar(char: Char): IO[Try[Unit]]

}

class ConsoleReadWriter extends ReadWriter {

  private val inSource: BufferedSource = Source.stdin

  override def readChar(): IO[Try[Option[Char]]] = IO { Try {
    if (inSource.hasNext) Some(inSource.next()) else None
  }}

  override def writeChar(char: Char): IO[Try[Unit]] = IO { Try {
    System.out.print(char)
  }}

}

object ConsoleReadWriter extends ConsoleReadWriter

class InMemoryReadWriter(input: String) extends ReadWriter {

  private var in: List[Char] = input.toCharArray.toList
  private var out: List[Char] = Nil

  override def readChar(): IO[Try[Option[Char]]] = IO { Try {
    in match {
      case Nil =>
        None
      case next :: rest =>
        in = rest
        Some(next)
    }
  }}

  override def writeChar(char: Char): IO[Try[Unit]] = IO { Try {
    out ::= char
  }}

  def finalOutput: String = String.valueOf(out.reverse.toArray)

}
