inThisBuild(
  Seq(
    sonatypeGithub := ("fommil", "scala-data-classes"),
    licenses := Seq(Apache2),
    scalaVersion := "2.12.1"
  )
)

libraryDependencies ++= shapeless.value
