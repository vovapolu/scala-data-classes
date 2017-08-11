package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import testing.meta._
import testing.{ caseclass, memoised, optimiseheap, weakmemoised }

class ApplyBenchmark {

  import BenchmarkData._

  // case class

  @Benchmark
  def caseClass(cs: CaseClassData): IndexedSeq[FooCaseClass[String]] =
    cs.data.map { case (a, b, c, d) => FooCaseClass(a, b, c, d) }

  @Benchmark
  def caseClassSpec(cs: CaseClassData): IndexedSeq[caseclass.Foo[String]] =
    cs.data.map { case (a, b, c, d) => caseclass.Foo(a, b, c, d) }

  @Benchmark
  def caseClassMeta(cs: CaseClassData): IndexedSeq[FooMeta[String]] =
    cs.data.map { case (a, b, c, d) => FooMeta(a, b, c, d) }

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass(
    oh: OptimizeHeapData
  ): IndexedSeq[FooOptimizeHeapCaseClass] =
    oh.data.map { case (a, b, c) => FooOptimizeHeapCaseClass(a, b, c) }

  @Benchmark
  def optimizeHeapSpec(oh: OptimizeHeapData): IndexedSeq[optimiseheap.Foo] =
    oh.data.map { case (a, b, c) => optimiseheap.Foo(a, b, c) }

  @Benchmark
  def optimizeHeapMeta(oh: OptimizeHeapData): IndexedSeq[FooMetaOptimiseHeap] =
    oh.data.map { case (a, b, c) => FooMetaOptimiseHeap(a, b, c) }

  // memoised

  @Benchmark
  def memoisedCaseClass(m: MemoisedData): IndexedSeq[FooMemoisedCaseClass] =
    m.data.map { case (a, b) => FooMemoisedCaseClass(a, b) }

  @Benchmark
  def memoisedSpec(m: MemoisedData): IndexedSeq[memoised.Foo] =
    m.data.map { case (a, b) => memoised.Foo(a, b) }

  @Benchmark
  def memoisedMeta(m: MemoisedData): IndexedSeq[FooMetaMemoised] =
    m.data.map { case (a, b) => FooMetaMemoised(a, b) }

  @Benchmark
  def memoisedWeak(m: MemoisedData): IndexedSeq[weakmemoised.Foo] =
    m.data.map { case (a, b) => weakmemoised.Foo(a, b) }
}
