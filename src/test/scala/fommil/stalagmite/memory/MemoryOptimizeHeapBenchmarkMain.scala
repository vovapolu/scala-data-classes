package fommil.stalagmite.memory

import _root_.scala._
import _root_.scala.Predef._
import fommil.stalagmite.{ data, TestUtils }

import scala.util.Random

object MemoryOptimizeHeapBenchmarkMain extends App {
  println("Running MemoryOptimizeHeapBenchmarkMain")

  final case class Foo(i: Option[Int],
                       s: Option[String],
                       b1: Option[Boolean],
                       b2: Option[Boolean],
                       b3: Option[Boolean],
                       b4: Option[Boolean])

  @data(
    optimiseHeapOptions = true,
    optimiseHeapStrings = true,
    optimiseHeapBooleans = true
  ) class FooMeta(i: Option[Int],
                  s: Option[String],
                  b1: Option[Boolean],
                  b2: Option[Boolean],
                  b3: Option[Boolean],
                  b4: Option[Boolean])

  def generateData = (1 to 10000).map(
    _ =>
      (
        TestUtils.randomNextOption(Random.nextInt(1000)),
        TestUtils.randomNextOption(Random.nextString(5)),
        TestUtils.randomNextOption(Random.nextBoolean()),
        TestUtils.randomNextOption(Random.nextBoolean()),
        TestUtils.randomNextOption(Random.nextBoolean()),
        TestUtils.randomNextOption(Random.nextBoolean())
    )
  )

  val foosMemory = TestUtils.measureMemoryConsumption("Case class") {
    generateData.map {
      case (a, b, c, d, e, f) => Foo(a, b, c, d, e, f)
    }
  }
  val foosMetaMemory = TestUtils.measureMemoryConsumption("Data class") {
    generateData.map {
      case (a, b, c, d, e, f) => FooMeta(a, b, c, d, e, f)
    }
  }
}
