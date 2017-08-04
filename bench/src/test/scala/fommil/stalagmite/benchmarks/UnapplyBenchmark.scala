package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import testing.meta._
import testing.{ caseclass, optimiseheap }

class UnapplyBenchmark {
  import BenchmarkData._

  // case class

  @Benchmark
  def caseClass(cs: CaseClassData): IndexedSeq[(Boolean, String, String, Int)] =
    cs.foos.map {
      case FooCaseClass(a, b, c, d) => (a, b, c, d)
    }

  @Benchmark
  def caseClassSpec(
    cs: CaseClassData
  ): IndexedSeq[(Boolean, String, String, Int)] =
    cs.foosSpec.map {
      case caseclass.Foo(a, b, c, d) => (a, b, c, d)
    }

  @Benchmark
  def caseClassMeta(
    cs: CaseClassData
  ): IndexedSeq[(Boolean, String, String, Int)] =
    cs.foosMeta.map {
      case FooMeta(a, b, c, d) => (a, b, c, d)
    }

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass(
    oh: OptimizeHeapData
  ): IndexedSeq[(Option[Boolean], Option[Boolean], Option[String])] =
    oh.foos.map {
      case FooOptimizeHeapCaseClass(a, b, c) => (a, b, c)
    }

  @Benchmark
  def optimizeHeapSpec(
    oh: OptimizeHeapData
  ): IndexedSeq[(Option[Boolean], Option[Boolean], Option[String])] =
    oh.foosSpec.map {
      case optimiseheap.Foo(a, b, c) => (a, b, c)
    }

  @Benchmark
  def optimizeHeapMeta(
    oh: OptimizeHeapData
  ): IndexedSeq[(Option[Boolean], Option[Boolean], Option[String])] =
    oh.foosMeta.map {
      case FooMetaOptimiseHeap(a, b, c) => (a, b, c)
    }
}
