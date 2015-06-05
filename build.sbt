name := "Categories"

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies := libraryDependencies.value ++ Seq(
   "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
   "org.specs2" %% "specs2-core" % "3.6" % "test",
   "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
   "com.novocode" % "junit-interface" % "0.11" % "test")

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")