package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import testing.meta._
import testing.{ caseclass, memoised, optimiseheap, weakmemoised }

class ApplyBenchmark {

  import BenchmarkData._

  //  jmh:run -i 15 -wi 15 -f1 -t10 .*ApplyBenchmark
  //  Benchmark                              Mode  Cnt      Score     Error  Units
  //  ApplyBenchmark.caseClass              thrpt   15  11450.948 ± 103.262  ops/s
  //  ApplyBenchmark.caseClassMeta          thrpt   15   8463.315 ±  40.761  ops/s
  //  ApplyBenchmark.caseClassSpec          thrpt   15   8477.634 ±  98.649  ops/s
  //  ApplyBenchmark.memoisedCaseClass      thrpt   15  12486.288 ± 395.986  ops/s
  //  ApplyBenchmark.memoisedMeta           thrpt   15   1026.234 ±   7.507  ops/s
  //  ApplyBenchmark.memoisedSpec           thrpt   15    587.448 ±   4.134  ops/s
  //  ApplyBenchmark.memoisedWeak           thrpt   15   1638.376 ±  95.335  ops/s
  //  ApplyBenchmark.optimizeHeapCaseClass  thrpt   15   7152.750 ± 289.303  ops/s
  //  ApplyBenchmark.optimizeHeapMeta       thrpt   15   1573.147 ±  85.862  ops/s
  //  ApplyBenchmark.optimizeHeapSpec       thrpt   15   1690.010 ±  69.412  ops/s

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
  def memoisedWeakSpec(m: MemoisedData): IndexedSeq[weakmemoised.Foo] =
    m.data.map { case (a, b) => weakmemoised.Foo(a, b) }
}
