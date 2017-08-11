package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import testing.meta._
import testing.{ caseclass, optimiseheap }

class UnapplyBenchmark {
  import BenchmarkData._

  // jmh:run -i 15 -wi 15 -f1 -t10 .*UnapplyBenchmark
  // Benchmark                                Mode  Cnt      Score     Error  Units
  // UnapplyBenchmark.caseClass              thrpt   15   7523.458 ±  76.971  ops/s
  // UnapplyBenchmark.caseClassMeta          thrpt   15   6542.607 ±  61.753  ops/s
  // UnapplyBenchmark.caseClassSpec          thrpt   15   6456.168 ± 165.521  ops/s
  // UnapplyBenchmark.optimizeHeapCaseClass  thrpt   15  15328.851 ± 380.972  ops/s
  // UnapplyBenchmark.optimizeHeapMeta       thrpt   15   1698.671 ±  62.760  ops/s
  // UnapplyBenchmark.optimizeHeapSpec       thrpt   15   1733.703 ±  11.927  ops/s

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
