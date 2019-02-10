name := "master_thesis_source"

version := "1.0"

// scalaVersion := "2.11.7"
scalaVersion := "2.12.1"

resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

scalacOptions ++= Seq(
  // "-Xfatal-warnings",
  //&"-Xfuture",
  //"-Xlint",
  // "-Yinline-warnings",
  //"-Yno-adapted-args",
  //"-Ywarn-adapted-args",
  //"-Ywarn-dead-code",
  //"-Ywarn-inaccessible",
  //"-Ywarn-infer-any",
  //"-Ywarn-nullary-override",
  //"-Ywarn-nullary-unit",
  //"-Ywarn-numeric-widen",
  //"-Ywarn-unused",
  //"-Ywarn-unused-import",
  //"-Ywarn-value-discard",
  //"-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked"
)

scalacOptions in (Compile, console) ~= (_ filterNot (Set("-Ywarn-unused-import","-Xfatal-warnings")))

scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value

addCompilerPlugin("org.spire-math" % "kind-projector_2.12" % "0.9.9")

libraryDependencies += "io.reactivex" % "rxscala_2.12" % "0.26.5"

libraryDependencies += "org.spire-math" %% "spire" % "0.13.0"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.17"

libraryDependencies += "commons-codec" % "commons-codec" % "1.10"

libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.0.5" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0"

libraryDependencies += "com.typesafe.akka" % "akka-actor_2.12" % "2.5.20"

libraryDependencies += "com.typesafe.akka" % "akka-remote_2.12" % "2.5.20"

libraryDependencies += "com.espertech" % "esper" % "5.2.0"

//libraryDependencies += "org.scala-lang" % "scala-swing" % "2.11.0-M7"
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.0-M2"

libraryDependencies += "com.storm-enroute" %% "scalameter" % "0.10.1" % "test"

//libraryDependencies += "org.pelotom" %% "effectful" % "1.2-SNAPSHOT"
lazy val effectfulProject = RootProject(uri("https://github.com/azanov/effectful.git"))
lazy val root = (project in file(".")).dependsOn(effectfulProject)

libraryDependencies += "io.argonaut" %% "argonaut" % "6.2.2"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.5"

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

// To avoid classloader problems with sbt console and native ope library
fork := true

// Otherwise problems with forking (too many)
parallelExecution in Test := false

// Forward stdinput of sbt to forked process for interactive use
connectInput in run := true
