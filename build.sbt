val scala3Version = "3.0.0"

lazy val commonSettings = Seq(
  version := "0.1",

  scalaVersion := scala3Version,

  libraryDependencies ++= Seq(
    "com.novocode" % "junit-interface" % "0.11" % "test"
  ),

  scalacOptions ++= Seq(
    "-language:implicitConversions",
    "-language:higherKinds",
  )
)

lazy val macros = project
  .in(file("modules/marcos"))
  .settings(
    name := "macros",
    commonSettings
  )

lazy val root = project
  .in(file("."))
  .dependsOn(macros)
  .settings(
    name := "Seminar Funtional Programming",
    commonSettings
  )
