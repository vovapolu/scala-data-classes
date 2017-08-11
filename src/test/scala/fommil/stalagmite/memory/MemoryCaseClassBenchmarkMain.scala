package fommil.stalagmite.memory

import _root_.scala._
import _root_.scala.Predef._
import fommil.stalagmite.{ data, TestUtils }

import scala.util.Random

// CAUTION: Don't run there benchmarks with `sbt "runMain ..."`
// GC in SBT may behave in really strange way,
// which results to large std error during memory usage measurements
// It's recommended to run memory benchmarks as standalone applications (using Idea or console)

object MemoryCaseClassBenchmarkMain extends App {

  //  Case class
  //    Iteration 1: consumed 89349 kb, totally 86719 kb ??? it's reproducible
  //    Iteration 2: consumed 89892 kb, totally 94064 kb ???
  //    Iteration 3: consumed 83816 kb, totally 95668 kb
  //    Iteration 4: consumed 81859 kb, totally 94866 kb
  //    Iteration 5: consumed 82235 kb, totally 94645 kb
  //
  //  Case class:
  //    mean - 85430 kb
  //    std  - 7799.87 kb
  //
  //  Data class
  //    Iteration 1: consumed 82441 kb, totally 82454 kb
  //    Iteration 2: consumed 82396 kb, totally 82065 kb
  //    Iteration 3: consumed 82345 kb, totally 82117 kb
  //    Iteration 4: consumed 82447 kb, totally 81903 kb
  //    Iteration 5: consumed 82405 kb, totally 81995 kb
  //
  //  Data class:
  //    mean - 82407 kb
  //    std  - 81.91 kb

  println("Running MemoryCaseClassBenchmark")

  final case class Foo(i: Int, b: Boolean, s: String)

  @data class FooMeta(i: Int, b: Boolean, s: String)

  def generateData = (1 to 1000000).map(
    _ =>
      (
        Random.nextInt(1000),
        Random.nextBoolean(),
        Random.nextString(5),
    )
  )

  TestUtils.measureMemoryConsumption("Case class") {
    generateData.map {
      case (a, b, c) => Foo(a, b, c)
    }
  }
  TestUtils.measureMemoryConsumption("Data class") {
    generateData.map {
      case (a, b, c) => FooMeta(a, b, c)
    }
  }
}
