name := "advent-of-code"

version := "0.1"

scalaVersion := "2.12.4"


resolvers += "Sonatype OSS Snapshots" at
  "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= {
  val scalaTestV = "3.0.1"
  Seq(
    "org.scalatest"       %% "scalatest"              % scalaTestV       % "test",
    "org.mockito"         %  "mockito-core"           % "2.11.0"         % "test",
    "com.storm-enroute" %% "scalameter" % "0.19-SNAPSHOT" % "test"

  )
}

testFrameworks += new TestFramework(
  "org.scalameter.ScalaMeterFramework")

logBuffered := false

parallelExecution in Test := false
