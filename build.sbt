name := "master_thesis_source"

version := "1.0"

scalaVersion := "2.11.6"

resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Yinline-warnings",
  "-Ywarn-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard"
)

libraryDependencies += "org.scalaz" % "scalaz-core_2.11" % "7.1.1"

libraryDependencies += "commons-codec" % "commons-codec" % "1.10"

addCompilerPlugin("org.spire-math" % "kind-projector_2.11" % "0.5.2")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

// To avoid classloader problems with sbt console and native ope library
fork := true

parallelExecution in Test := false
