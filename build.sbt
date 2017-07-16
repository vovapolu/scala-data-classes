inThisBuild(
  Seq(
    sonatypeGithub := ("fommil", "stalagmite"),
    licenses := Seq(Apache2),
    scalaVersion := "2.12.2"
  )
)

lazy val macroAnnotationSettings = Seq(
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.bintrayRepo("scalameta", "maven"),
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M9" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  scalacOptions in (Compile, console) := Seq() // macroparadise plugin doesn't work in repl yet.
)

libraryDependencies ++= Seq(
 "com.google.guava" % "guava" % "22.0" % "test",
 "org.ensime" %% "pcplod" % "1.2.1" % "test",
 "org.typelevel" %% "cats" % "0.9.0" % "test",
 "org.typelevel" %% "kittens" % "1.0.0-M10" % "test",
 "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.6" % "test",
 "com.github.fommil" %% "spray-json-shapeless" % "1.4.0" % "test",
 "org.scalameta" %% "testkit" % "1.8.0" % "test",
 "org.scalameta" %% "scalameta" % "1.8.0" % Provided
) ++ shapeless.value.map(_ % "test")

macroAnnotationSettings

javaOptions in Test ++= Seq(
  s"""-Dpcplod.settings=${(scalacOptions in Test).value.mkString(",")}""",
  s"""-Dpcplod.classpath=${(fullClasspath in Test).value.map(_.data).mkString(",")}"""
)

scalacOptions in Test ++= Seq(
  "-Yno-imports",
  "-Yno-predef"
)
