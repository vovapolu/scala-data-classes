package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import testing.meta._
import testing.{ caseclass, optimiseheap }

class UnapplyBenchmark {
  import BenchmarkData._

  // case class

  @Benchmark
  def caseClass: IndexedSeq[(Boolean, String, String, Int)] =
    fooCaseClasses.map {
      case FooCaseClass(a, b, c, d) => (a, b, c, d)
    }

  @Benchmark
  def caseClassSpec: IndexedSeq[(Boolean, String, String, Int)] =
    fooCaseClassesSpec.map {
      case caseclass.Foo(a, b, c, d) => (a, b, c, d)
    }

  @Benchmark
  def caseClassMeta: IndexedSeq[(Boolean, String, String, Int)] =
    fooCaseClassesMeta.map {
      case FooMeta(a, b, c, d) => (a, b, c, d)
    }

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass
    : IndexedSeq[(Option[Boolean], Option[Boolean], Option[String])] =
    fooOptimizeHeapCaseClasses.map {
      case FooOptimizeHeapCaseClass(a, b, c) => (a, b, c)
    }

  @Benchmark
  def optimizeHeapSpec
    : IndexedSeq[(Option[Boolean], Option[Boolean], Option[String])] =
    fooOptimizeHeapSpec.map {
      case optimiseheap.Foo(a, b, c) => (a, b, c)
    }

  @Benchmark
  def optimizeHeapMeta
    : IndexedSeq[(Option[Boolean], Option[Boolean], Option[String])] =
    fooOptimizeHeapMeta.map {
      case FooMetaOptimiseHeap(a, b, c) => (a, b, c)
    }
}
