lazy val brainfuck = (project in file(".")).settings(
  name := "Brainfuck",
  version := "1.0",
  scalaVersion := "2.12.4",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.4" % Test
  )
)
