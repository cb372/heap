import sbt._
import Keys._

object HeapBuild extends Build {

  lazy val project = Project(
    id = "heap",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "heap",
      scalaVersion := "2.10.2",
      version := "0.1-SNAPSHOT",
      libraryDependencies ++= Seq(
        "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
      )
    )
  )

}
