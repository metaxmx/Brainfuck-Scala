package com.illucit.bf

import com.illucit.bf.BrainFuck.{FinalExecution, Terminated}
import org.scalatest.{Ignore, Matchers, WordSpec}

import scala.io.Source

@Ignore
class LongRunningBrainFuckTests extends WordSpec with Matchers {

  "The BrainFuck interpreter also" should {

    "print self-portrait" in {
      val program: String = Source.fromResource("self-portrait.b").mkString
      val readWriter: InMemoryReadWriter = new InMemoryReadWriter("")
      val result: FinalExecution = BrainFuck(readWriter = readWriter).execute(program).run
      result shouldBe Terminated
      readWriter.finalOutput shouldBe program.replace("\n\r", "\n").drop(program.indexOf('\n') + 1)
    }

  }

}
