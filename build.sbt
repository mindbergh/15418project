name := "BFI"

version := "1.0"

scalaVersion := "2.11.6"


libraryDependencies <++= (scalaVersion)(sv =>
  Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
    "org.scala-lang" % "scala-reflect" % "2.11.6",
    "org.scala-lang" % "scala-compiler" % "2.11.6",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
  )
)