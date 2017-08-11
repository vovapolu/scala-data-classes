package fommil.stalagmite.memory

import _root_.scala._
import _root_.scala.Predef._
import fommil.stalagmite.{ data, TestUtils }

import scala.util.Random

// CAUTION: Don't run there benchmarks with `sbt "runMain ..."`
// GC in SBT may behave in really strange way,
// which results to large std error during memory usage measurements
// It's recommended to run memory benchmarks as standalone applications (using Idea or console)

object MemoryOptimizeHeapBenchmarkMain extends App {

  //  Case class
  //    Iteration 1: consumed 128706 kb, totally 126076 kb
  //    Iteration 2: consumed 125702 kb, totally 127134 kb
  //    Iteration 3: consumed 125958 kb, totally 128778 kb
  //    Iteration 4: consumed 124990 kb, totally 129093 kb
  //    Iteration 5: consumed 126137 kb, totally 132440 kb
  //
  //  Case class:
  //    mean - 126299 kb
  //    std  - 2829.60 kb
  //
  //  Data class
  //    Iteration 1: consumed 51483 kb, totally 51454 kb
  //    Iteration 2: consumed 51402 kb, totally 51567 kb
  //    Iteration 3: consumed 51408 kb, totally 51543 kb
  //    Iteration 4: consumed 51398 kb, totally 51514 kb
  //    Iteration 5: consumed 51400 kb, totally 51503 kb
  //
  //  Data class:
  //    mean - 51418 kb
  //    std  - 72.62 kb

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

  def generateData = (1 to 1000000).map(
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
