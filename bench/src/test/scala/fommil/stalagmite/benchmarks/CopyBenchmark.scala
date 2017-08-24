package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

import testing.meta._
import testing.{ caseclass, memoised, optimiseheap, weakmemoised }

class CopyBenchmark {
  import BenchmarkData._

  // case class

  def mapWithSomeValue[T, S, V](
    foos: IndexedSeq[T]
  )(initVal: V, newVal: V => V)(f: (T, V) => S): Seq[S] = {

    def go(curInd: Int, curVal: V): List[S] =
      if (curInd >= foos.length) {
        List.empty[S]
      } else {
        f(foos(curInd), curVal) :: go(curInd + 1, newVal(curVal))
      }

    go(0, initVal)
  }

  @Benchmark
  def caseClass(
    cs: CaseClassData
  ): Seq[(FooCaseClass[String], FooCaseClass[Int])] =
    mapWithSomeValue(cs.foos)(0, (i: Int) => i + 1) {
      case (foo, i) => (foo.copy(i = i), foo.copy(t = i))
    }

  @Benchmark
  def caseClassSpec(
    cs: CaseClassData
  ): Seq[(caseclass.Foo[String], caseclass.Foo[Int])] =
    mapWithSomeValue(cs.foosSpec)(0, (i: Int) => i + 1) {
      case (foo, i) => (foo.copy(i = i), foo.copy(t = i))
    }

  @Benchmark
  def caseClassMeta(
    cs: CaseClassData
  ): Seq[(FooMeta[String], FooMeta[Int])] =
    mapWithSomeValue(cs.foosMeta)(0, (i: Int) => i + 1) {
      case (foo, i) => (foo.copy(i = i), foo.copy(t = i))
    }

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass(
    oh: OptimizeHeapData
  ): Seq[(FooOptimizeHeapCaseClass, FooOptimizeHeapCaseClass)] =
    mapWithSomeValue(oh.foos)(false, (b: Boolean) => !b) {
      case (foo, b) => (foo.copy(a = Some(b)), foo.copy(b = None))
    }

  @Benchmark
  def optimizeHeapSpec(
    oh: OptimizeHeapData
  ): Seq[(optimiseheap.Foo, optimiseheap.Foo)] =
    mapWithSomeValue(oh.foosSpec)(false, (b: Boolean) => !b) {
      case (foo, b) => (foo.copy(a = Some(b)), foo.copy(b = None))
    }

  @Benchmark
  def optimizeHeapMeta(
    oh: OptimizeHeapData
  ): Seq[(FooMetaOptimiseHeap, FooMetaOptimiseHeap)] =
    mapWithSomeValue(oh.foosMeta)(false, (b: Boolean) => !b) {
      case (foo, b) => (foo.copy(a = Some(b)), foo.copy(b = None))
    }

  // memoised

  @Benchmark
  def memoisedCaseClass(
    m: MemoisedData
  ): Seq[(FooMemoisedCaseClass, FooMemoisedCaseClass)] =
    mapWithSomeValue(m.foos)(
      (false, 0.toChar),
      (t: (Boolean, Char)) => (!t._1, (t._2 + 1).toChar)
    ) {
      case (foo, (b, c)) =>
        (foo.copy(b = b), foo.copy(s = String.valueOf(c)))
    }

  @Benchmark
  def memoisedSpec(m: MemoisedData): Seq[(memoised.Foo, memoised.Foo)] =
    mapWithSomeValue(m.foosSpec)(
      (false, 0.toChar),
      (t: (Boolean, Char)) => (!t._1, (t._2 + 1).toChar)
    ) {
      case (foo, (b, c)) =>
        (foo.copy(a = b), foo.copy(s = String.valueOf(c)))
    }

  @Benchmark
  def memoisedMeta(
    m: MemoisedData
  ): Seq[(FooMetaMemoised, FooMetaMemoised)] =
    mapWithSomeValue(m.foosMeta)(
      (false, 0.toChar),
      (t: (Boolean, Char)) => (!t._1, (t._2 + 1).toChar)
    ) {
      case (foo, (b, c)) =>
        (foo.copy(a = b), foo.copy(s = String.valueOf(c)))
    }

  @Benchmark
  def memoisedWeakSpec(
    m: MemoisedData
  ): Seq[(weakmemoised.Foo, weakmemoised.Foo)] =
    mapWithSomeValue(m.foosWeakSpec)(
      (false, 0.toChar),
      (t: (Boolean, Char)) => (!t._1, (t._2 + 1).toChar)
    ) {
      case (foo, (b, c)) =>
        (foo.copy(a = b), foo.copy(s = String.valueOf(c)))
    }
}
