package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

class ToStringBenchmark {
  import BenchmarkData._

  // jmh:run -i 15 -wi 15 -f1 -t10 .*ToStringBenchmark
  // Benchmark                                 Mode  Cnt      Score      Error  Units
  // ToStringBenchmark.caseClass              thrpt   15    436.456 ±  107.668  ops/s
  // ToStringBenchmark.caseClassMeta          thrpt   15    362.077 ±   41.060  ops/s
  // ToStringBenchmark.caseClassSpec          thrpt   15    385.432 ±   21.921  ops/s
  // ToStringBenchmark.memoisedCaseClass      thrpt   15    669.668 ±   72.259  ops/s
  // ToStringBenchmark.memoisedMeta           thrpt   15  20532.909 ±  634.209  ops/s
  // ToStringBenchmark.memoisedSpec           thrpt   15  19573.501 ± 1125.749  ops/s
  // ToStringBenchmark.memoisedWeak           thrpt   15  20033.298 ±  435.380  ops/s
  // ToStringBenchmark.optimizeHeapCaseClass  thrpt   15    354.627 ±   56.703  ops/s
  // ToStringBenchmark.optimizeHeapMeta       thrpt   15    347.920 ±   43.600  ops/s
  // ToStringBenchmark.optimizeHeapSpec       thrpt   15    312.148 ±   45.332  ops/s

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
