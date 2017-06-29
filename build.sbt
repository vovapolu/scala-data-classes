inThisBuild(
  Seq(
    sonatypeGithub := ("fommil", "stalagmite"),
    licenses := Seq(Apache2),
    scalaVersion := "2.12.1"
  )
)

libraryDependencies ++= Seq(
 "com.google.guava" % "guava" % "21.0" % "test",
 "org.ensime" %% "pcplod" % "1.2.0" % "test",
 "org.typelevel" %% "cats" % "0.9.0" % "test",
 "org.typelevel" %% "kittens" % "1.0.0-M9" % "test",
 "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.4" % "test",
 "com.github.fommil" %% "spray-json-shapeless" % "1.3.0" % "test"
) ++ shapeless.value.map(_ % "test")

javaOptions in Test ++= Seq(
  s"""-Dpcplod.settings=${(scalacOptions in Test).value.mkString(",")}""",
  s"""-Dpcplod.classpath=${(fullClasspath in Test).value.map(_.data).mkString(",")}"""
)
