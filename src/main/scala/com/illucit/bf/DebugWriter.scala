package com.illucit.bf

trait DebugWriter {

  def writeDebugLine(line: => String): IO[Unit] = IO {
    Console.err.println(line)
  }

}
