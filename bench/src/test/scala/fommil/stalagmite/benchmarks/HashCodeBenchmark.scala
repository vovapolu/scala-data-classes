package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

class HashCodeBenchmark {
  import BenchmarkData._

  // case class

  @Benchmark
  def caseClass: IndexedSeq[Int] = fooCaseClasses.map(_.hashCode)

  @Benchmark
  def caseClassSpec: IndexedSeq[Int] = fooCaseClassesSpec.map(_.hashCode)

  @Benchmark
  def caseClassMeta: IndexedSeq[Int] = fooCaseClassesMeta.map(_.hashCode)

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass: IndexedSeq[Int] =
    fooOptimizeHeapCaseClasses.map(_.hashCode)

  @Benchmark
  def optimizeHeapSpec: IndexedSeq[Int] = fooOptimizeHeapSpec.map(_.hashCode)

  @Benchmark
  def optimizeHeapMeta: IndexedSeq[Int] = fooOptimizeHeapMeta.map(_.hashCode)

  // memoised

  @Benchmark
  def memoisedCaseClass: IndexedSeq[Int] =
    fooMemoisedCaseClasses.map(_.hashCode)

  @Benchmark
  def memoisedSpec: IndexedSeq[Int] = fooMemoisedSpec.map(_.hashCode)

  @Benchmark
  def memoisedMeta: IndexedSeq[Int] = fooMemoisedMeta.map(_.hashCode)
}
