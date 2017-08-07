package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.{ Scope, Setup, State }
import shapeless.{ cachedImplicit, Generic }
import testing.meta._
import testing.{ caseclass, memoised, optimiseheap }

import scala.util.Random

object BenchmarkData {
  case class FooCaseClass[T](b: Boolean, s: String, t: T, i: Int)
  case class FooOptimizeHeapCaseClass(a: Option[Boolean],
                                      b: Option[Boolean],
                                      s: Option[String])
  case class FooMemoisedCaseClass(b: Boolean, s: String)

  def generateWithDuplicates[T](generator: Unit => T,
                                genCount: Int,
                                duplicatesCount: Int): IndexedSeq[T] = {
    val data = (1 to genCount).map(_ => generator(()))
    data ++ (1 to duplicatesCount).map(_ => data(Random.nextInt(data.length)))
  }

  val dataSize        = 10000
  val duplicatesRatio = 0.2

  @State(Scope.Benchmark)
  class CaseClassData {
    var data: IndexedSeq[(Boolean, String, String, Int)] = _
    var foos: IndexedSeq[FooCaseClass[String]]           = _
    var foosSpec: IndexedSeq[caseclass.Foo[String]]      = _
    var foosMeta: IndexedSeq[FooMeta[String]]            = _

    @Setup
    def setup(): Unit = {
      Random.setSeed(0xBABE)
      data = generateWithDuplicates(
        _ =>
          (Random.nextBoolean,
           Random.nextString(10),
           Random.nextString(20),
           Random.nextInt),
        (dataSize * (1 - duplicatesRatio)).toInt,
        (dataSize * duplicatesRatio).toInt
      )
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
      Random.setSeed(0xCAFE)
      data = {
        def nextOption[T](value: => T, someProb: Double = 0.5): Option[T] =
          if (Random.nextDouble < someProb) {
            Some(value)
          } else {
            None
          }

        generateWithDuplicates(
          _ =>
            (nextOption(Random.nextBoolean),
             nextOption(Random.nextBoolean),
             nextOption(Random.nextString(10))),
          (dataSize * (1 - duplicatesRatio)).toInt,
          (dataSize * duplicatesRatio).toInt
        )
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

    @Setup
    def setup(): Unit = {
      Random.setSeed(0xFEEL)
      data = {
        generateWithDuplicates(
          _ => (Random.nextBoolean, Random.nextString(10)),
          (dataSize * (1 - duplicatesRatio)).toInt,
          (dataSize * duplicatesRatio).toInt
        )
      }
      foos = data.map { case (a, b)     => FooMemoisedCaseClass(a, b) }
      foosSpec = data.map { case (a, b) => memoised.Foo(a, b) }
      foosMeta = data.map { case (a, b) => FooMetaMemoised(a, b) }
    }
  }

  implicit val G: Generic[FooCaseClass[String]]      = cachedImplicit
  implicit val GSpec: Generic[caseclass.Foo[String]] = cachedImplicit
  implicit val GMeta: Generic[FooMeta[String]]       = cachedImplicit

  @State(Scope.Benchmark)
  class EqualsData {
    var comparingPairs: IndexedSeq[(Int, Int)] = _

    @Setup
    def setup(): Unit =
      comparingPairs = (1 to math.pow(dataSize.toDouble, 1.5).toInt)
        .map(_ => (Random.nextInt(dataSize), Random.nextInt(dataSize)))
  }
}
