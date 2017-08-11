package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

class HashCodeBenchmark {
  import BenchmarkData._

  // jmh:run -i 15 -wi 15 -f1 -t10 .*HashCodeBenchmark
  // Benchmark                                 Mode  Cnt      Score     Error  Units
  // HashCodeBenchmark.caseClass              thrpt   15   6104.730 ± 315.642  ops/s
  // HashCodeBenchmark.caseClassMeta          thrpt   15   6752.413 ±  83.813  ops/s
  // HashCodeBenchmark.caseClassSpec          thrpt   15   6880.228 ±  84.879  ops/s
  // HashCodeBenchmark.memoisedCaseClass      thrpt   15   7546.154 ± 209.177  ops/s
  // HashCodeBenchmark.memoisedMeta           thrpt   15  12395.199 ± 109.503  ops/s
  // HashCodeBenchmark.memoisedSpec           thrpt   15  12922.047 ± 373.775  ops/s
  // HashCodeBenchmark.memoisedWeak           thrpt   15  12636.120 ± 265.068  ops/s
  // HashCodeBenchmark.optimizeHeapCaseClass  thrpt   15   2761.036 ± 167.080  ops/s
  // HashCodeBenchmark.optimizeHeapMeta       thrpt   15   1540.516 ±  10.557  ops/s
  // HashCodeBenchmark.optimizeHeapSpec       thrpt   15   1554.675 ±  10.978  ops/s

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
  def memoisedWeak(m: MemoisedData): IndexedSeq[Int] =
    m.foosWeak.map(_.hashCode)
}
