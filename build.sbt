name := "master_thesis_source"

version := "1.0"

scalaVersion := "2.11.6"

resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xfuture",
  "-Xlint",
  "-Yinline-warnings",
  "-Yno-adapted-args",
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

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.6"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.2"

libraryDependencies += "commons-codec" % "commons-codec" % "1.10"

addCompilerPlugin("org.spire-math" % "kind-projector_2.11" % "0.5.2")

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.11" % "2.3.10"

libraryDependencies += "com.typesafe.akka" % "akka-remote_2.11" % "2.3.10"

// To avoid classloader problems with sbt console and native ope library
fork := true

// Otherwise problems with forking (too many)
parallelExecution in Test := false
