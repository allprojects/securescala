name := "master_thesis_source"

version := "1.0"

scalaVersion := "2.11.6"

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

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
