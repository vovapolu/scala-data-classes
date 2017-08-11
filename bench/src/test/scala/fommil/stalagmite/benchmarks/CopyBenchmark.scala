package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

import testing.meta._
import testing.{ caseclass, memoised, optimiseheap, weakmemoised }
import scala.util.Random

class CopyBenchmark {
  import BenchmarkData._

  // case class

  @Benchmark
  def caseClass(
    cs: CaseClassData
  ): IndexedSeq[(FooCaseClass[String], FooCaseClass[Int])] =
    cs.foos.map(
      foo => (foo.copy(i = Random.nextInt), foo.copy(t = Random.nextInt))
    )

  @Benchmark
  def caseClassSpec(
    cs: CaseClassData
  ): IndexedSeq[(caseclass.Foo[String], caseclass.Foo[Int])] =
    cs.foosSpec.map(
      foo => (foo.copy(i = Random.nextInt), foo.copy(t = Random.nextInt))
    )

  @Benchmark
  def caseClassMeta(
    cs: CaseClassData
  ): IndexedSeq[(FooMeta[String], FooMeta[Int])] =
    cs.foosMeta.map(
      foo => (foo.copy(i = Random.nextInt), foo.copy(t = Random.nextInt))
    )

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass(
    oh: OptimizeHeapData
  ): IndexedSeq[(FooOptimizeHeapCaseClass, FooOptimizeHeapCaseClass)] =
    oh.foos.map(
      foo => (foo.copy(a = Some(Random.nextBoolean)), foo.copy(b = None))
    )

  @Benchmark
  def optimizeHeapSpec(
    oh: OptimizeHeapData
  ): IndexedSeq[(optimiseheap.Foo, optimiseheap.Foo)] =
    oh.foosSpec.map(
      foo => (foo.copy(a = Some(Random.nextBoolean)), foo.copy(b = None))
    )

  @Benchmark
  def optimizeHeapMeta(
    oh: OptimizeHeapData
  ): IndexedSeq[(FooMetaOptimiseHeap, FooMetaOptimiseHeap)] =
    oh.foosMeta.map(
      foo => (foo.copy(a = Some(Random.nextBoolean)), foo.copy(b = None))
    )

  // memoised

  @Benchmark
  def memoisedCaseClass(
    m: MemoisedData
  ): IndexedSeq[(FooMemoisedCaseClass, FooMemoisedCaseClass)] =
    m.foos.map(
      foo =>
        (foo.copy(b = Random.nextBoolean), foo.copy(s = Random.nextString(1)))
    )

  @Benchmark
  def memoisedSpec(m: MemoisedData): IndexedSeq[(memoised.Foo, memoised.Foo)] =
    m.foosSpec.map(
      foo =>
        (foo.copy(a = Random.nextBoolean), foo.copy(s = Random.nextString(1)))
    )

  @Benchmark
  def memoisedMeta(
    m: MemoisedData
  ): IndexedSeq[(FooMetaMemoised, FooMetaMemoised)] =
    m.foosMeta.map(
      foo =>
        (foo.copy(a = Random.nextBoolean), foo.copy(s = Random.nextString(1)))
    )

  @Benchmark
  def memoisedWeak(
    m: MemoisedData
  ): IndexedSeq[(weakmemoised.Foo, weakmemoised.Foo)] =
    m.foosWeak.map(
      foo =>
        (foo.copy(a = Random.nextBoolean), foo.copy(s = Random.nextString(1)))
    )
}
