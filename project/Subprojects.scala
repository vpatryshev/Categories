import sbt._

object Subprojects {
  lazy val scala2 = Project(id = "scala2", base = file("scala2"))
  lazy val scala3 = Project(id = "scala3", base = file("scala3"))
}