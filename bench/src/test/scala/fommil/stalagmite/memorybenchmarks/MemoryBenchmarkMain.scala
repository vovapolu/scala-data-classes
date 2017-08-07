package fommil.stalagmite.memorybenchmarks

import fommil.stalagmite.data

import scala.util.Random

case class Foo(i: Int, b: Boolean, s: String)
@data class FooMeta(i: Int, b: Boolean, s: String)

object MemoryCaseClassBenchmarkMain extends App {

  println("Running MemoryCaseClassBenchmark")

  val data = (1 to 100000).map(
    _ =>
      (
        Random.nextInt(),
        Random.nextBoolean(),
        Random.nextString(10)
    )
  )

  val foos = data.map {
    case (a, b, c) => Foo(a, b, c)
  }
  val foosMeta = data.map {
    case (a, b, c) => Foo(a, b, c)
  }
}
