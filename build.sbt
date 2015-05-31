name := "Categories"

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies += "org.specs2" %% "specs2-core" % "3.6" % "test"

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")
