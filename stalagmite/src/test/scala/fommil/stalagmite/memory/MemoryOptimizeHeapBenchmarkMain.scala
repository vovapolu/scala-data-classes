package fommil.stalagmite.memory

import fommil.stalagmite.TestUtils.MeduimString

import _root_.scala._
import _root_.scala.Predef._
import fommil.stalagmite.data
import fommil.stalagmite.TestUtils._

import org.scalacheck._
import Arbitrary.arbitrary

import org.scalacheck.rng.Seed
import shapeless.tag.@@

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

  val generator =
    Gen.listOfN(500000,
                arbitrary[
                  (Option[Int],
                   Option[String @@ MeduimString],
                   Option[Boolean],
                   Option[Boolean],
                   Option[Boolean],
                   Option[Boolean])
                ])

  def generateData =
    generator(Gen.Parameters.default, Seed(0xBABE2)).getOrElse(List.empty)

  prettyPrintResults(
    "Case class",
    measureMemoryConsumption() {
      generateData.map {
        case (a, b, c, d, e, f) => Foo(a, b, c, d, e, f)
      }
    }
  )
  prettyPrintResults(
    "Data class",
    measureMemoryConsumption() {
      generateData.map {
        case (a, b, c, d, e, f) => FooMeta(a, b, c, d, e, f)
      }
    }
  )
}
