package com.illucit.bf

import com.illucit.bf.BrainFuck.{FinalExecution, Terminated}
import org.scalatest.{Matchers, WordSpec}

import scala.io.Source

class BrainFuckTest extends WordSpec with Matchers {

  "The BrainFuck interpreter" should {

    "handle hello world" in {
      val program: String = Source.fromResource("helloworld.b").mkString
      val readWriter: InMemoryReadWriter = new InMemoryReadWriter("")
      val result: FinalExecution = BrainFuck(readWriter = readWriter).execute(program).run
      result shouldBe Terminated
      readWriter.finalOutput shouldBe "Hello World!\n"
    }

    "handle quines" in {
      val program: String = Source.fromResource("quine.b").mkString
      val readWriter: InMemoryReadWriter = new InMemoryReadWriter("")
      val result: FinalExecution = BrainFuck(readWriter = readWriter).execute(program).run
      result shouldBe Terminated
      readWriter.finalOutput shouldBe program.replaceAll("\n|\r", "")
    }

    "find out cell size for 8-bit" in {
      val program: String = Source.fromResource("cellsize.b").mkString
      val readWriterBytes: InMemoryReadWriter = new InMemoryReadWriter("")
      val resultBytes: FinalExecution = BrainFuck(readWriter = readWriterBytes, alphabet = Alphabet.`8-bit`).execute(program).run
      resultBytes shouldBe Terminated
      readWriterBytes.finalOutput shouldBe "8 bit cells\n"
    }

    "find out cell size for 16-bit" in {
      val program: String = Source.fromResource("cellsize.b").mkString
      val readWriterUnicode: InMemoryReadWriter = new InMemoryReadWriter("")
      val resultUnicode: FinalExecution = BrainFuck(readWriter = readWriterUnicode, alphabet = Alphabet.`16-bit`).execute(program).run
      resultUnicode shouldBe Terminated
      readWriterUnicode.finalOutput shouldBe "16 bit cells\n"
    }

    "find out cell size for 32-bit" in {
      val program: String = Source.fromResource("cellsize.b").mkString
      val readWriterUnicode: InMemoryReadWriter = new InMemoryReadWriter("")
      val resultUnicode: FinalExecution = BrainFuck(readWriter = readWriterUnicode, alphabet = Alphabet.`32-bit`).execute(program).run
      resultUnicode shouldBe Terminated
      readWriterUnicode.finalOutput shouldBe "32 bit cells\n"
    }

    "print the ascii set" in {
      val program: String = Source.fromResource("asciichar.b").mkString
      val readWriter: InMemoryReadWriter = new InMemoryReadWriter("")
      val result: FinalExecution = BrainFuck(readWriter = readWriter, alphabet = Alphabet.`8-bit`).execute(program).run
      result shouldBe Terminated
      readWriter.finalOutput shouldBe Range(0, 256).map(_.toByte.toChar).mkString
    }

    "calculate pi" in {
      val program: String = Source.fromResource("pi.b").mkString
      val readWriter: InMemoryReadWriter = new InMemoryReadWriter("")
      val result: FinalExecution = BrainFuck(readWriter = readWriter, alphabet = Alphabet.`8-bit`).execute(program).run
      result shouldBe Terminated
      readWriter.finalOutput shouldBe "3.14070455282885\n"
    }

    "count words" in {
      val program: String = Source.fromResource("wordcount.b").mkString
      val readWriter: InMemoryReadWriter = new InMemoryReadWriter("this is first line\nand second lin\n\nthird\0")
      val result: FinalExecution = BrainFuck(readWriter = readWriter, alphabet = Alphabet.`8-bit`).execute(program).run
      result shouldBe Terminated
      readWriter.finalOutput shouldBe "     3       8       40"
    }

  }

}
