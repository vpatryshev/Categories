name := "Categories, scala 2"

version := "2.0"

scalaVersion := "2.13.6"

maxErrors := 10

watchTriggeredMessage := Watch.clearScreenOnTrigger

libraryDependencies := libraryDependencies.value ++ Seq(
   "com.novocode" % "junit-interface" % "0.11" % "test")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.2.0-M2"
libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "4.12.0" % Test withSources()

scalacOptions ++= Seq("-deprecation", "-feature")

Test/scalacOptions ++= Seq("-Yrangepos")
