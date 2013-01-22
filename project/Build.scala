import sbt._
import Keys._

object TrailsBuild extends Build with BuildSettings {
  lazy val root = Project(id = "root", base = file("."), settings = standardSettings) aggregate(core, blueprint, neo4j)

  lazy val core = Project(id = "core", base = file("core"), settings = standardSettings ++ Seq(
    name := "trails core",
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"
    )
  ))

  lazy val blueprint = Project(id = "blueprint", base = file("blueprint"), settings = standardSettings ++ Seq(
    name := "trails blueprint",
    version := "0.1",
    libraryDependencies ++= Seq(
      "com.tinkerpop.blueprints" % "blueprints-core" % "2.1.0"
    )
  )) dependsOn(core % "test->test;compile->compile")

  lazy val neo4j = Project(id = "neo4j", base = file("neo4j"), settings = standardSettings ++ Seq(
    name := "trails neo4j",
    version := "0.1",
    libraryDependencies ++= Seq(
      "org.neo4j" % "neo4j-kernel" % "1.8.1",
      "org.neo4j" % "neo4j-kernel-test" % "1.8.1" % "test"
    )
  )) dependsOn(core % "test->test;compile->compile")
}

trait BuildSettings {
  final val SCALA_VERSION = "2.10.0"

  val standardSettings = Defaults.defaultSettings ++
    Seq(
      organization := "ch.fhnw.imvs",
      scalaVersion := SCALA_VERSION,
      scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-target:jvm-1.7", "-Xlint")
    )
}

