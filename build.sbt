name := "advent-of-code"

version := "0.1"

scalaVersion := "2.12.4"


libraryDependencies ++= {
  val scalaTestV = "3.0.1"
  Seq(
    "org.scalatest"       %% "scalatest"              % scalaTestV       % "test",
    "org.mockito"         %  "mockito-core"           % "2.11.0"         % "test"
  )
}