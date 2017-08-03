package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

import testing.meta._
import testing.{ caseclass, memoised, optimiseheap }
import scala.util.Random

class CopyBenchmark {
  import BenchmarkData._

  // case class

  @Benchmark
  def caseClass: IndexedSeq[(FooCaseClass[String], FooCaseClass[Int])] =
    fooCaseClasses.map(
      foo => (foo.copy(i = Random.nextInt), foo.copy(t = Random.nextInt))
    )

  @Benchmark
  def caseClassSpec: IndexedSeq[(caseclass.Foo[String], caseclass.Foo[Int])] =
    fooCaseClassesSpec.map(
      foo => (foo.copy(i = Random.nextInt), foo.copy(t = Random.nextInt))
    )

  @Benchmark
  def caseClassMeta: IndexedSeq[(FooMeta[String], FooMeta[Int])] =
    fooCaseClassesMeta.map(
      foo => (foo.copy(i = Random.nextInt), foo.copy(t = Random.nextInt))
    )

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass
    : IndexedSeq[(FooOptimizeHeapCaseClass, FooOptimizeHeapCaseClass)] =
    fooOptimizeHeapCaseClasses.map(
      foo => (foo.copy(a = Some(Random.nextBoolean)), foo.copy(b = None))
    )

  @Benchmark
  def optimizeHeapSpec: IndexedSeq[(optimiseheap.Foo, optimiseheap.Foo)] =
    fooOptimizeHeapSpec.map(
      foo => (foo.copy(a = Some(Random.nextBoolean)), foo.copy(b = None))
    )

  @Benchmark
  def optimizeHeapMeta: IndexedSeq[(FooMetaOptimiseHeap, FooMetaOptimiseHeap)] =
    fooOptimizeHeapMeta.map(
      foo => (foo.copy(a = Some(Random.nextBoolean)), foo.copy(b = None))
    )

  // memoised

  @Benchmark
  def memoisedCaseClass
    : IndexedSeq[(FooMemoisedCaseClass, FooMemoisedCaseClass)] =
    fooMemoisedCaseClasses.map(
      foo =>
        (foo.copy(b = Random.nextBoolean), foo.copy(s = Random.nextString(1)))
    )

  @Benchmark
  def memoisedSpec: IndexedSeq[(memoised.Foo, memoised.Foo)] =
    fooMemoisedSpec.map(
      foo =>
        (foo.copy(a = Random.nextBoolean), foo.copy(s = Random.nextString(1)))
    )

  @Benchmark
  def memoisedMeta: IndexedSeq[(FooMetaMemoised, FooMetaMemoised)] =
    fooMemoisedMeta.map(
      foo =>
        (foo.copy(a = Random.nextBoolean), foo.copy(s = Random.nextString(1)))
    )
}
