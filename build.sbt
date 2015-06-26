name := "Categories"

version := "1.0"

scalaVersion := "2.11.1"

maxErrors := 10

triggeredMessage := Watched.clearWhenTriggered

libraryDependencies := libraryDependencies.value ++ Seq(
   "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3",
   "org.specs2" %% "specs2-core" % "3.6" % "test",
   "com.novocode" % "junit-interface" % "0.11" % "test")

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"

scalacOptions in Test ++= Seq("-Yrangepos")

javacOptions in (Compile, compile) += "-Xlint"
