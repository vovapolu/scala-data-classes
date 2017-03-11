inThisBuild(
  Seq(
    sonatypeGithub := ("fommil", "scala-data-classes"),
    licenses := Seq(Apache2),
    scalaVersion := "2.12.1"
  )
)

libraryDependencies ++= Seq(
 "org.ensime" %% "pcplod" % "1.2.0" % "test",
 "org.typelevel" %% "cats" % "0.9.0" % "test",
 "org.typelevel" %% "kittens" % "1.0.0-M9"
) ++ shapeless.value.map(_ % "test")

javaOptions in Test ++= Seq(
  s"""-Dpcplod.settings=${(scalacOptions in Test).value.mkString(",")}""",
  s"""-Dpcplod.classpath=${(fullClasspath in Test).value.map(_.data).mkString(",")}"""
)
