name := "PresidentAI"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"
libraryDependencies += "org.scala-lang.modules" % "scala-swing_2.13" % "2.1.1"

// deeplearning4j dependencies
libraryDependencies += "org.deeplearning4j" % "deeplearning4j-core" % "1.0.0-beta6"
libraryDependencies += "org.nd4j" % "nd4j-native-platform" % "1.0.0-beta6"


jacocoExcludes in Test := Seq(
  "ui.*",
  "utils.*",
  "game.Game",

)
