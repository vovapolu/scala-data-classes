package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.{ Scope, Setup, State }
import shapeless.{ cachedImplicit, Generic }
import testing.meta._
import testing.{ caseclass, memoised, optimiseheap, weakmemoised }
import org.scalacheck._
import Arbitrary.arbitrary
import org.scalacheck.rng.Seed
import shapeless.tag.@@

import scala.util.Random

object BenchmarkData {
  case class FooCaseClass[T](b: Boolean, s: String, t: T, i: Int)
  case class FooOptimizeHeapCaseClass(a: Option[Boolean],
                                      b: Option[Boolean],
                                      s: Option[String])
  case class FooMemoisedCaseClass(b: Boolean, s: String)

  import fommil.stalagmite.TestUtils._

  private[this] val dataSize        = 10000
  private[this] val duplicatesRatio = 0.2

  @State(Scope.Benchmark)
  class CaseClassData {
    var data: IndexedSeq[(Boolean, String, String, Int)] = _
    var foos: IndexedSeq[FooCaseClass[String]]           = _
    var foosSpec: IndexedSeq[caseclass.Foo[String]]      = _
    var foosMeta: IndexedSeq[FooMeta[String]]            = _

    @Setup
    def setup(): Unit = {
      data = vectorWithDuplicates(
        arbitrary[
          (Boolean, String @@ MeduimString, String @@ MeduimString, Int)
        ],
        (dataSize * (1 - duplicatesRatio)).toInt,
        (dataSize * duplicatesRatio).toInt
      ).apply(Gen.Parameters.default, Seed(0xCAFEL)).getOrElse(Vector.empty)
      foos = data.map { case (a, b, c, d)     => FooCaseClass(a, b, c, d) }
      foosSpec = data.map { case (a, b, c, d) => caseclass.Foo(a, b, c, d) }
      foosMeta = data.map { case (a, b, c, d) => FooMeta(a, b, c, d) }
    }
  }

  @State(Scope.Benchmark)
  class OptimizeHeapData {
    var data: IndexedSeq[(Option[Boolean], Option[Boolean], Option[String])] = _
    var foos: IndexedSeq[FooOptimizeHeapCaseClass]                           = _
    var foosSpec: IndexedSeq[optimiseheap.Foo]                               = _
    var foosMeta: IndexedSeq[FooMetaOptimiseHeap]                            = _

    @Setup
    def setup(): Unit = {
      data = {
        vectorWithDuplicates(
          arbitrary[
            (Option[Boolean], Option[Boolean], Option[String @@ MeduimString])
          ],
          (dataSize * (1 - duplicatesRatio)).toInt,
          (dataSize * duplicatesRatio).toInt
        ).apply(Gen.Parameters.default, Seed(0xCAFE2L)).getOrElse(Vector.empty)
      }

      foos = data.map {
        case (a, b, c) => FooOptimizeHeapCaseClass(a, b, c)
      }
      foosSpec = data.map { case (a, b, c) => optimiseheap.Foo(a, b, c) }
      foosMeta = data.map { case (a, b, c) => FooMetaOptimiseHeap(a, b, c) }
    }
  }

  @State(Scope.Benchmark)
  class MemoisedData {
    var data: IndexedSeq[(Boolean, String)]    = _
    var foos: IndexedSeq[FooMemoisedCaseClass] = _
    var foosSpec: IndexedSeq[memoised.Foo]     = _
    var foosMeta: IndexedSeq[FooMetaMemoised]  = _
    var foosWeak: IndexedSeq[weakmemoised.Foo] = _

    @Setup
    def setup(): Unit = {
      Random.setSeed(0xFEEL)
      data = {
        vectorWithDuplicates(
          arbitrary[
            (Boolean, String @@ MeduimString)
          ],
          (dataSize * (1 - duplicatesRatio)).toInt,
          (dataSize * duplicatesRatio).toInt
        ).apply(Gen.Parameters.default, Seed(0xCAFE3L)).getOrElse(Vector.empty)
      }
      foos = data.map { case (a, b)     => FooMemoisedCaseClass(a, b) }
      foosSpec = data.map { case (a, b) => memoised.Foo(a, b) }
      foosMeta = data.map { case (a, b) => FooMetaMemoised(a, b) }
      foosWeak = data.map { case (a, b) => weakmemoised.Foo(a, b) }
    }
  }

  implicit val G: Generic[FooCaseClass[String]]      = cachedImplicit
  implicit val GSpec: Generic[caseclass.Foo[String]] = cachedImplicit
  implicit val GMeta: Generic[FooMeta[String]]       = cachedImplicit

  @State(Scope.Benchmark)
  class EqualsData {
    var comparingPairs: Seq[(Int, Int)] = _

    @Setup
    def setup(): Unit = {
      val pairsGen = Gen
        .listOfN(
          math.pow(dataSize.toDouble, 1.5).toInt,
          Gen.zip(Gen.choose(0, dataSize), Gen.choose(0, dataSize))
        )

      comparingPairs =
        pairsGen(Gen.Parameters.default, Seed(0xCAFE4L)).getOrElse(List.empty)
    }
  }
}
