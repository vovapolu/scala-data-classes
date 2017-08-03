package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import testing.meta._
import testing.{ caseclass, memoised, optimiseheap }

class ApplyBenchmark {

  import BenchmarkData._

  // case class

  @Benchmark
  def caseClass: IndexedSeq[FooCaseClass[String]] =
    data.map { case (a, b, c, d) => FooCaseClass(a, b, c, d) }

  @Benchmark
  def caseClassSpec: IndexedSeq[caseclass.Foo[String]] =
    data.map { case (a, b, c, d) => caseclass.Foo(a, b, c, d) }

  @Benchmark
  def caseClassMeta: IndexedSeq[FooMeta[String]] =
    data.map { case (a, b, c, d) => FooMeta(a, b, c, d) }

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass: IndexedSeq[FooOptimizeHeapCaseClass] =
    dataOptimizeHeap.map { case (a, b, c) => FooOptimizeHeapCaseClass(a, b, c) }

  @Benchmark
  def optimizeHeapSpec: IndexedSeq[optimiseheap.Foo] =
    dataOptimizeHeap.map { case (a, b, c) => optimiseheap.Foo(a, b, c) }

  @Benchmark
  def optimizeHeapMeta: IndexedSeq[FooMetaOptimiseHeap] =
    dataOptimizeHeap.map { case (a, b, c) => FooMetaOptimiseHeap(a, b, c) }

  // memoised

  @Benchmark
  def memoisedCaseClass: IndexedSeq[FooMemoisedCaseClass] =
    dataMemoised.map { case (a, b) => FooMemoisedCaseClass(a, b) }

  @Benchmark
  def memoisedSpec: IndexedSeq[memoised.Foo] =
    dataMemoised.map { case (a, b) => memoised.Foo(a, b) }

  @Benchmark
  def memoisedMeta: IndexedSeq[FooMetaMemoised] =
    dataMemoised.map { case (a, b) => FooMetaMemoised(a, b) }
}
