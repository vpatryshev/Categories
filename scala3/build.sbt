name := "Categories, scala 3"

version := "3.0"

scalaVersion := "3.0.0"
crossScalaVersions ++= Seq("2.13.6", "3.0.0")

maxErrors := 10

watchTriggeredMessage := Watch.clearScreenOnTrigger

libraryDependencies := libraryDependencies.value ++ Seq(
   "com.novocode" % "junit-interface" % "0.11" % "test")

libraryDependencies += ("org.scala-lang.modules" %% "scala-parser-combinators" % "1.2.0-M1").cross(CrossVersion.for3Use2_13)
libraryDependencies += ("org.specs2" %% "specs2-scalacheck" % "4.12.0" % Test).cross(CrossVersion.for3Use2_13) withSources()

scalacOptions ++= Seq("-deprecation", "-feature")


//scalacOptions ++= Seq(
//  "-deprecation",
//  "UTF-8",
//  "-feature")

//scalacOptions ++= {
//   Seq(
//      "-encoding",
//      "UTF-8",
//      "-feature",
//      "-language:implicitConversions",
//      // disabled during the migration
//      // "-Xfatal-warnings"
//   ) ++
//     (CrossVersion.partialVersion(scalaVersion.value) match {
//        case Some((3, _)) => Seq(
//           "-unchecked",
//           "-source:3.0-migration"
//        )
//        case _ => Seq(
//           "-deprecation",
//           "-Xfatal-warnings",
//           "-Wunused:imports,privates,locals",
//           "-Wvalue-discard"
//        )
//     })
//}