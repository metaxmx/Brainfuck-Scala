package com.illucit.bf

import scala.io.Source

object Main extends App {

  args.toList match {
    case file :: Nil => println(BrainFuck().execute(Source.fromFile(file).mkString).run)
    case _  => println("Usage: brainfuck <program.b>")
  }

}
