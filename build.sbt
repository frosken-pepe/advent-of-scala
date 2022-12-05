lazy val root = project
  .in(file("."))
  .settings(
    name := "advent-of-scala",
    version := "0.1",
    scalaVersion := "2.13.10",
    libraryDependencies += "com.typesafe.play" %% "play-json" % "2.10.0-RC5"
  )
