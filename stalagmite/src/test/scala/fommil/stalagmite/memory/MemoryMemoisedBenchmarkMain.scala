package fommil.stalagmite.memory

import _root_.scala._
import _root_.scala.Predef._
import fommil.stalagmite.data
import fommil.stalagmite._
import fommil.stalagmite.TestUtils._

import org.scalacheck._
import Arbitrary.arbitrary

import org.scalacheck.rng.Seed
import shapeless.tag.@@

import testing.weakmemoised

// CAUTION: Don't run there benchmarks with `sbt "runMain ..."`
// GC in SBT may behave in really strange way,
// which results to large std error during memory usage measurements
// It's recommended to run memory benchmarks as standalone applications (using Idea or console)

object MemoryMemoisedBenchmarkMain extends App {

  //  ---- Repeating data (good case for memosation) ----
  //
  //  Case class
  //    Iteration 1: consumed 77782 kb, totally 75007 kb
  //    Iteration 2: consumed 78460 kb, totally 78630 kb
  //    Iteration 3: consumed 74674 kb, totally 78456 kb
  //    Iteration 4: consumed 74865 kb, totally 78472 kb
  //    Iteration 5: consumed 74890 kb, totally 78513 kb
  //
  //  Case class:
  //    mean - 76134 kb
  //    std  - 3663.35 kb
  //
  //  Data class with strong memoisation
  //    Iteration 1: consumed 17960 kb, totally 17980 kb
  //    Iteration 2: consumed 4543 kb, totally 17717 kb
  //    Iteration 3: consumed 4539 kb, totally 17720 kb
  //    Iteration 4: consumed 4538 kb, totally 17722 kb
  //    Iteration 5: consumed 4537 kb, totally 17724 kb
  //
  //  Data class with strong memoisation:
  //    mean - 7224 kb
  //    std  - 12003.91 kb
  //
  //  Data class with internal caching
  //    Iteration 1: consumed 16880 kb, totally 16881 kb
  //    Iteration 2: consumed 4537 kb, totally 16879 kb
  //    Iteration 3: consumed 4536 kb, totally 16880 kb
  //    Iteration 4: consumed 4536 kb, totally 16880 kb
  //    Iteration 5: consumed 4536 kb, totally 16880 kb
  //
  //  Data class with internal caching:
  //    mean - 7005 kb
  //    std  - 11040.89 kb
  //
  //  Data class with weak memoisation
  //    Iteration 1: consumed 17681 kb, totally 17682 kb
  //    Iteration 2: consumed 12310 kb, totally 17659 kb
  //    Iteration 3: consumed 12311 kb, totally 17660 kb
  //    Iteration 4: consumed 12311 kb, totally 17660 kb
  //    Iteration 5: consumed 12310 kb, totally 17659 kb
  //
  //  Data class with weak memoisation:
  //    mean - 13385 kb
  //    std  - 4803.82 kb
  //
  //  Weak memoisation spec
  //    Iteration 1: consumed 32694 kb, totally 32694 kb
  //    Iteration 2: consumed 20062 kb, totally 34847 kb
  //    Iteration 3: consumed 20071 kb, totally 34833 kb
  //    Iteration 4: consumed 20076 kb, totally 34823 kb
  //    Iteration 5: consumed 20080 kb, totally 34817 kb
  //
  //  Weak memoisation spec:
  //    mean - 22597 kb
  //    std  - 11289.22 kb
  //
  //  ---- Distinct data (bad case for memosation) ----
  //
  //  Case class
  //    Iteration 1: consumed 74496 kb, totally 74496 kb
  //    Iteration 2: consumed 74843 kb, totally 74491 kb
  //    Iteration 3: consumed 74803 kb, totally 77193 kb
  //    Iteration 4: consumed 74819 kb, totally 77163 kb
  //    Iteration 5: consumed 74832 kb, totally 77146 kb
  //
  //  Case class:
  //    mean - 74759 kb
  //    std  - 294.97 kb
  //
  //  Data class with strong memoisation
  //    Iteration 1: consumed 120549 kb, totally 120550 kb
  //    Iteration 2: consumed 122059 kb, totally 238073 kb
  //    Iteration 3: consumed 113868 kb, totally 347405 kb
  //    Iteration 4: consumed 127224 kb, totally 470092 kb
  //    Iteration 5: consumed 114905 kb, totally 580462 kb
  //
  //  Data class with strong memoisation:
  //    mean - 119721 kb
  //    std  - 10949.59 kb
  //
  //  Strong memoisation with String caching
  //    Iteration 1: consumed 147830 kb, totally 147830 kb
  //    Iteration 2: consumed 154268 kb, totally 297563 kb
  //    Iteration 3: consumed 138295 kb, totally 431086 kb
  //    Iteration 4: consumed 170201 kb, totally 596751 kb
  //    Iteration 5: consumed 137780 kb, totally 729995 kb
  //
  //  Strong memoisation with String caching:
  //    mean - 149675 kb
  //    std  - 26773.13 kb
  //
  //  Data class with weak memoisation
  //    Iteration 1: consumed 117234 kb, totally 117235 kb
  //    Iteration 2: consumed 75374 kb, totally 117905 kb
  //    Iteration 3: consumed 75211 kb, totally 118273 kb
  //    Iteration 4: consumed 75159 kb, totally 118670 kb
  //    Iteration 5: consumed 75108 kb, totally 118936 kb
  //
  //  Data class with weak memoisation:
  //    mean - 83617 kb
  //    std  - 37585.73 kb
  //
  //  Weak memoisation spec
  //    Iteration 1: consumed 316307 kb, totally 316307 kb
  //    Iteration 2: consumed 170672 kb, totally 314495 kb
  //    Iteration 3: consumed 172081 kb, totally 314095 kb
  //    Iteration 4: consumed 172424 kb, totally 314032 kb
  //    Iteration 5: consumed 172471 kb, totally 314022 kb
  //
  //  Weak memoisation spec:
  //    mean - 200791 kb
  //    std  - 129159.12 kb

  println("Running MemoryMemoisedBenchmarkMain")

  final case class Foo(b: Boolean, s: String)

  @data(
    memoise = true,
    memoiseStrong = true
  )
  class FooMeta(b: Boolean, s: String)

  @data(
    memoise = true
  )
  class FooMetaWeak(b: Boolean, s: String)

  @data(
    memoise = true,
    memoiseStrong = true,
    memoiseRefs = Seq('s)
  )
  class FooMetaWithRefs(b: Boolean, s: String)

  val repeatingGenerator =
    Gen.listOfN(100000, arbitrary[(Boolean, String @@ SmallString)])
  val distinctGenerator =
    Gen.listOfN(100000, arbitrary[(Boolean, String @@ MeduimString)])

  def generateRepeatingData =
    repeatingGenerator.sample.getOrElse(List())

  def generateDistinctData =
    distinctGenerator.sample.getOrElse(List())

  def mapData[T](data: () => Seq[(Boolean, String)],
                 mapF: (Boolean, String) => T): Seq[T] =
    data().map {
      case (a, b) => mapF(a, b)
    }

  for {
    (text, data) <- Seq(
                     ("---- Repeating data (good case for memosation) ----",
                      () => generateRepeatingData),
                     ("---- Distinct data (bad case for memosation) ----",
                      () => generateDistinctData)
                   )
  } {
    println(text)
    println()

    prettyPrintResults("Case class", measureMemoryConsumption() {
      mapData(data, Foo)
    })

    prettyPrintResults("Data class with strong memoisation",
                       measureMemoryConsumption() {
                         mapData(data, FooMeta.apply)
                       })
    prettyPrintResults("Strong memoisation with String caching",
                       measureMemoryConsumption() {
                         mapData(data, FooMetaWithRefs.apply)
                       })
    prettyPrintResults("Data class with weak memoisation",
                       measureMemoryConsumption() {
                         mapData(data, FooMetaWeak.apply)
                       })
    prettyPrintResults("Weak memoisation spec", measureMemoryConsumption() {
      mapData(data, weakmemoised.Foo.apply)
    })
  }
}
