package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

class ToStringBenchmark {
  import BenchmarkData._

  // case class

  @Benchmark
  def caseClass(cs: CaseClassData): IndexedSeq[String] = cs.foos.map(_.toString)

  @Benchmark
  def caseClassSpec(cs: CaseClassData): IndexedSeq[String] =
    cs.foosSpec.map(_.toString)

  @Benchmark
  def caseClassMeta(cs: CaseClassData): IndexedSeq[String] =
    cs.foosMeta.map(_.toString)

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass(oh: OptimizeHeapData): IndexedSeq[String] =
    oh.foos.map(_.toString)

  @Benchmark
  def optimizeHeapSpec(oh: OptimizeHeapData): IndexedSeq[String] =
    oh.foosSpec.map(_.toString)

  @Benchmark
  def optimizeHeapMeta(oh: OptimizeHeapData): IndexedSeq[String] =
    oh.foosMeta.map(_.toString)

  // memoised

  @Benchmark
  def memoisedCaseClass(m: MemoisedData): IndexedSeq[String] =
    m.foos.map(_.toString)

  @Benchmark
  def memoisedSpec(m: MemoisedData): IndexedSeq[String] =
    m.foosSpec.map(_.toString)

  @Benchmark
  def memoisedMeta(m: MemoisedData): IndexedSeq[String] =
    m.foosMeta.map(_.toString)

  @Benchmark
  def memoisedWeak(m: MemoisedData): IndexedSeq[String] =
    m.foosWeak.map(_.toString)
}
