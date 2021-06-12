name := "Categories"

version := "1.0"

scalaVersion := "2.13.6"
//scalaVersion := "3.0.0"
//crossScalaVersions ++= Seq("2.12", "3.0.0")

maxErrors := 10

triggeredMessage := Watched.clearWhenTriggered

libraryDependencies := libraryDependencies.value ++ Seq(
   "com.novocode" % "junit-interface" % "0.11" % "test")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.2.0-M2"
libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "4.12.0" % Test withSources()

scalacOptions ++= Seq("-deprecation", "-feature")

scalacOptions in Test ++= Seq("-Yrangepos")

javacOptions in (Compile, compile) += "-Xlint"
