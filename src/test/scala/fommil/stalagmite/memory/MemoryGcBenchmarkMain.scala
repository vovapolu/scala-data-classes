package fommil.stalagmite.memory

import _root_.scala._
import _root_.scala.Predef._
import fommil.stalagmite.{ data, TestUtils }

import testing.weakmemoised
import scala.util.Random

// CAUTION: Don't run there benchmarks with `sbt "runMain ..."`
// GC in SBT may behave in really strange way,
// which results to large std error during memory usage measurements
// It's recommended to run memory benchmarks as standalone applications (using Idea or console)

object MemoryGcBenchmarkMain extends App {
  //  ---- Repeating data (good case for memosation) ----
  //
  //  Strong memoisation
  //    Iteration 1: consumed 16460 kb, totally 14165 kb
  //    Iteration 2: consumed 3861 kb, totally 17678 kb
  //    Iteration 3: consumed 66 kb, totally 17654 kb
  //    Iteration 4: consumed 74 kb, totally 17638 kb
  //    Iteration 5: consumed 80 kb, totally 17627 kb
  //
  //  Strong memoisation:
  //    mean - 4108 kb
  //    std  - 14193.77 kb
  //
  //  Strong memoisation with internal caching
  //    Iteration 1: consumed 12437 kb, totally 12235 kb
  //    Iteration 2: consumed 87 kb, totally 12229 kb
  //    Iteration 3: consumed 87 kb, totally 12226 kb
  //    Iteration 4: consumed 88 kb, totally 12224 kb
  //    Iteration 5: consumed 89 kb, totally 12224 kb
  //
  //  Strong memoisation with internal caching:
  //    mean - 2558 kb
  //    std  - 11045.20 kb
  //
  //  Weak memoisation
  //    Iteration 1: consumed 6691 kb, totally 6691 kb
  //    Iteration 2: consumed 1395 kb, totally 6687 kb
  //    Iteration 3: consumed 1387 kb, totally 6697 kb
  //    Iteration 4: consumed 1384 kb, totally 6706 kb
  //    Iteration 5: consumed 1384 kb, totally 6714 kb
  //
  //  Weak memoisation:
  //    mean - 2448 kb
  //    std  - 4742.96 kb
  //
  //  Weak memoisation spec
  //    Iteration 1: consumed 18593 kb, totally 18593 kb
  //    Iteration 2: consumed 2996 kb, totally 18565 kb
  //    Iteration 3: consumed 3005 kb, totally 18548 kb
  //    Iteration 4: consumed 3003 kb, totally 18531 kb
  //    Iteration 5: consumed 3009 kb, totally 18527 kb
  //
  //  Weak memoisation spec:
  //    mean - 6121 kb
  //    std  - 13943.89 kb
  //
  //  ---- Distinct data (bad case for memosation) ----
  //
  //  Strong memoisation
  //    Iteration 1: consumed 119552 kb, totally 119552 kb
  //    Iteration 2: consumed 116905 kb, totally 236366 kb
  //    Iteration 3: consumed 109400 kb, totally 345676 kb
  //    Iteration 4: consumed 124981 kb, totally 470566 kb
  //    Iteration 5: consumed 109666 kb, totally 580142 kb
  //
  //  Strong memoisation:
  //    mean - 116101 kb
  //  std  - 13331.36 kb
  //
  //  Strong memoisation with internal caching
  //    Iteration 1: consumed 142670 kb, totally 142671 kb
  //    Iteration 2: consumed 150420 kb, totally 293000 kb
  //    Iteration 3: consumed 133533 kb, totally 426443 kb
  //    Iteration 4: consumed 164911 kb, totally 591255 kb
  //    Iteration 5: consumed 133146 kb, totally 724310 kb
  //
  //  Strong memoisation with internal caching:
  //    mean - 144936 kb
  //    std  - 26518.15 kb
  //
  //  Weak memoisation
  //    Iteration 1: consumed 46322 kb, totally 46322 kb
  //    Iteration 2: consumed -431 kb, totally 44393 kb -- unfortunate gc call
  //    Iteration 3: consumed 1527 kb, totally 45130 kb
  //    Iteration 4: consumed 1516 kb, totally 45150 kb
  //    Iteration 5: consumed 1510 kb, totally 45163 kb
  //
  //  Weak memoisation:
  //    mean - 10089 kb
  //    std  - 40544.90 kb
  //
  //  Weak memoisation spec
  //    Iteration 1: consumed 146916 kb, totally 146916 kb
  //    Iteration 2: consumed 3730 kb, totally 148202 kb
  //    Iteration 3: consumed 3023 kb, totally 147776 kb
  //    Iteration 4: consumed 3131 kb, totally 147146 kb
  //    Iteration 5: consumed 3538 kb, totally 147234 kb
  //
  //  Weak memoisation spec:
  //    mean - 32068 kb
  //    std  - 128405.33 kb

  println("Running MemoryMemoisedBenchmarkMain")

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

  def generateRepeatingData = (1 to 1000000).map(
    _ =>
      (
        Random.nextBoolean(),
        Random.nextString(1),
    )
  )

  def generateDistinctData = (1 to 1000000).map(
    _ =>
      (
        Random.nextBoolean(),
        Random.nextString(2),
    )
  )

  def mapData[T](data: () => IndexedSeq[(Boolean, String)],
                 mapF: (Boolean, String) => T): IndexedSeq[T] =
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

    TestUtils.measureMemoryConsumption("Strong memoisation") {
      val res = mapData(data, FooMeta.apply)
      res.take(10000) ++ res.takeRight(10000)
    }
    TestUtils.measureMemoryConsumption(
      "Strong memoisation with internal caching"
    ) {
      val res = mapData(data, FooMetaWithRefs.apply)
      res.take(10000) ++ res.takeRight(10000)
    }
    TestUtils.measureMemoryConsumption("Weak memoisation") {
      val res = mapData(data, FooMetaWeak.apply)
      res.take(10000) ++ res.takeRight(10000)
    }
    TestUtils.measureMemoryConsumption("Weak memoisation spec") {
      val res = mapData(data, weakmemoised.Foo.apply)
      res.take(10000) ++ res.takeRight(10000)
    }
  }
}
