name := "Categories, scala 3"

version := "3.0"

scalaVersion := "3.6.4"

maxErrors := 10

watchTriggeredMessage := Watch.clearScreenOnTrigger

libraryDependencies := libraryDependencies.value ++ Seq(
   "com.novocode" % "junit-interface" % "0.11" % "test")

libraryDependencies += ("org.scala-lang.modules" %% "scala-parser-combinators" % "1.2.0-M1").cross(CrossVersion.for3Use2_13)
libraryDependencies += ("org.specs2" %% "specs2-scalacheck" % "4.12.0" % Test).cross(CrossVersion.for3Use2_13) withSources()

scalacOptions ++= Seq("-deprecation", "-feature")

Test / fork := true
Test / testForkedParallel := true


