import sbt.{Build, Project, ProjectRef, uri}

object MyBuild extends Build {

  lazy val root = Project("root", file(".")) dependsOn(effectfulProject)
  lazy val effectfulProject =
       RootProject(uri("https://github.com/azanov/effectful.git"))

}