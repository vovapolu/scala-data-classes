package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import testing.meta._
import testing.{ caseclass, memoised, optimiseheap, weakmemoised }

import scala.collection.mutable

class HashSetBenchmark {
  import BenchmarkData._

  // case class

  @Benchmark
  def caseClass(cs: CaseClassData): mutable.HashSet[FooCaseClass[String]] =
    mutable.HashSet(cs.foos: _*)

  @Benchmark
  def caseClassSpec(cs: CaseClassData): mutable.HashSet[caseclass.Foo[String]] =
    mutable.HashSet(cs.foosSpec: _*)

  @Benchmark
  def caseClassMeta(cs: CaseClassData): mutable.HashSet[FooMeta[String]] =
    mutable.HashSet(cs.foosMeta: _*)

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass(
    oh: OptimizeHeapData
  ): mutable.HashSet[FooOptimizeHeapCaseClass] =
    mutable.HashSet(oh.foos: _*)

  @Benchmark
  def optimizeHeapSpec(
    oh: OptimizeHeapData
  ): mutable.HashSet[optimiseheap.Foo] =
    mutable.HashSet(oh.foosSpec: _*)

  @Benchmark
  def optimizeHeapMeta(
    oh: OptimizeHeapData
  ): mutable.HashSet[FooMetaOptimiseHeap] =
    mutable.HashSet(oh.foosMeta: _*)

  // memoised

  @Benchmark
  def memoisedCaseClass(
    m: MemoisedData
  ): mutable.HashSet[FooMemoisedCaseClass] =
    mutable.HashSet(m.foos: _*)

  @Benchmark
  def memoisedSpec(m: MemoisedData): mutable.HashSet[memoised.Foo] =
    mutable.HashSet(m.foosSpec: _*)

  @Benchmark
  def memoisedMeta(m: MemoisedData): mutable.HashSet[FooMetaMemoised] =
    mutable.HashSet(m.foosMeta: _*)

  @Benchmark
  def memoisedWeakSpec(m: MemoisedData): mutable.HashSet[weakmemoised.Foo] =
    mutable.HashSet(m.foosWeakSpec: _*)

  @Benchmark
  def memoisedWeak(m: MemoisedData): mutable.HashSet[FooMetaMemoisedWeak] =
    mutable.HashSet(m.foosWeak: _*)

  @Benchmark
  def memoisedIntern(m: MemoisedData): mutable.HashSet[FooMetaInternMemoised] =
    mutable.HashSet(m.foosInternMeta: _*)
}
