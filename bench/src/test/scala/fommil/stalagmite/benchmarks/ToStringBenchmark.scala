package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

class ToStringBenchmark {
  import BenchmarkData._

  // case class

  @Benchmark
  def caseClass: IndexedSeq[String] = fooCaseClasses.map(_.toString)

  @Benchmark
  def caseClassSpec: IndexedSeq[String] = fooCaseClassesSpec.map(_.toString)

  @Benchmark
  def caseClassMeta: IndexedSeq[String] = fooCaseClassesMeta.map(_.toString)

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass: IndexedSeq[String] =
    fooOptimizeHeapCaseClasses.map(_.toString)

  @Benchmark
  def optimizeHeapSpec: IndexedSeq[String] = fooOptimizeHeapSpec.map(_.toString)

  @Benchmark
  def optimizeHeapMeta: IndexedSeq[String] = fooOptimizeHeapMeta.map(_.toString)

  // memoised

  @Benchmark
  def memoisedCaseClass: IndexedSeq[String] =
    fooMemoisedCaseClasses.map(_.toString)

  @Benchmark
  def memoisedSpec: IndexedSeq[String] = fooMemoisedSpec.map(_.toString)

  @Benchmark
  def memoisedMeta: IndexedSeq[String] = fooMemoisedMeta.map(_.toString)
}
