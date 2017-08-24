package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

class HashCodeBenchmark {
  import BenchmarkData._

  // case class

  @Benchmark
  def caseClass(cs: CaseClassData): IndexedSeq[Int] =
    cs.foos.map(_.hashCode)

  @Benchmark
  def caseClassSpec(cs: CaseClassData): IndexedSeq[Int] =
    cs.foosSpec.map(_.hashCode)

  @Benchmark
  def caseClassMeta(cs: CaseClassData): IndexedSeq[Int] =
    cs.foosMeta.map(_.hashCode)

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass(oh: OptimizeHeapData): IndexedSeq[Int] =
    oh.foos.map(_.hashCode)

  @Benchmark
  def optimizeHeapSpec(oh: OptimizeHeapData): IndexedSeq[Int] =
    oh.foosSpec.map(_.hashCode)

  @Benchmark
  def optimizeHeapMeta(oh: OptimizeHeapData): IndexedSeq[Int] =
    oh.foosMeta.map(_.hashCode)

  // memoised

  @Benchmark
  def memoisedCaseClass(m: MemoisedData): IndexedSeq[Int] =
    m.foos.map(_.hashCode)

  @Benchmark
  def memoisedSpec(m: MemoisedData): IndexedSeq[Int] =
    m.foosSpec.map(_.hashCode)

  @Benchmark
  def memoisedMeta(m: MemoisedData): IndexedSeq[Int] =
    m.foosMeta.map(_.hashCode)

  @Benchmark
  def memoisedWeakSpec(m: MemoisedData): IndexedSeq[Int] =
    m.foosWeakSpec.map(_.hashCode)
}
