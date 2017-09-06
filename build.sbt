inThisBuild(
  Seq(
    organization := "com.fommil",
    sonatypeGithub := ("fommil", "stalagmite"),
    licenses := Seq(LGPL3),
    scalaVersion := "2.12.3"
  )
)

enablePlugins(NeoJmh)
inConfig(Jmh)(
  sensibleTestSettings ++
    scalafmtSettings ++
    HeaderPlugin.toBeScopedSettings
)

libraryDependencies ++= Seq(
  "com.google.guava"           % "guava" % "22.0" % "test",
  "com.google.guava"           % "guava-testlib" % "18.0" % "test",
  "org.ensime"                 %% "pcplod" % "1.2.1" % "test",
  "org.typelevel"              %% "cats" % "0.9.0" % "test",
  "org.typelevel"              %% "kittens" % "1.0.0-M10" % "test",
  "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % "test",
  "com.github.fommil"          %% "spray-json-shapeless" % "1.4.0" % "test",
  "org.scalameta"              %% "testkit" % "1.8.0" % "test",
  "org.scalameta"              %% "scalameta" % "1.8.0" % Provided
) ++ shapeless.value.map(_     % "test")

addCompilerPlugin(
  "org.scalameta" % "paradise" % "3.0.0-M10" cross CrossVersion.full
)
scalacOptions += "-Xplugin-require:macroparadise"

javaOptions in Test ++= Seq(
  s"""-Dpcplod.settings=${(scalacOptions in Test).value.mkString(",")}""",
  s"""-Dpcplod.classpath=${(fullClasspath in Test).value
    .map(_.data)
    .mkString(",")}"""
)
scalacOptions in Test ++= Seq(
  "-Yno-imports",
  "-Yno-predef"
)
scalacOptions in Jmh -= "-Yno-imports"
scalacOptions in Jmh -= "-Yno-predef"

scalafmtOnCompile in ThisBuild := true
scalafmtConfig in ThisBuild := file("project/scalafmt.conf")
scalafmtVersion in ThisBuild := "1.1.0"

addCommandAlias("fmt", ";sbt:scalafmt ;scalafmt ;test:scalafmt ;jmh:scalafmt")

// WORKAROUND https://github.com/scalameta/paradise/issues/10
scalacOptions in (Compile, console) ~= (_ filterNot (_ contains "paradise"))
// WORKAROUND https://github.com/scalameta/paradise/issues/216
sources in (Compile, doc) := Nil

wartremoverWarnings in (Compile, compile) := Seq(
  Wart.AsInstanceOf,
  Wart.EitherProjectionPartial,
  Wart.IsInstanceOf,
  Wart.TraversableOps,
  Wart.NonUnitStatements,
  Wart.Null,
  Wart.OptionPartial,
  Wart.Return,
  Wart.StringPlusAny,
  Wart.Throw,
  Wart.TryPartial,
  Wart.Var,
  Wart.FinalCaseClass,
  Wart.ExplicitImplicitTypes
)
wartremoverWarnings in (Test, compile) := Seq(
  Wart.EitherProjectionPartial,
  Wart.TraversableOps,
  Wart.Return,
  Wart.StringPlusAny,
  Wart.TryPartial,
  Wart.FinalCaseClass,
  Wart.ExplicitImplicitTypes
)
