package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

class FieldAccessBenchmark {
  import BenchmarkData._

  // jmh:run -i 15 -wi 15 -f1 -t10 .*FieldAccessBenchmark
  // Benchmark                                    Mode  Cnt      Score     Error  Units
  // FieldAccessBenchmark.caseClass              thrpt   15   7528.583 ± 147.026  ops/s
  // FieldAccessBenchmark.caseClassMeta          thrpt   15   6903.130 ± 782.015  ops/s
  // FieldAccessBenchmark.caseClassSpec          thrpt   15   7194.162 ± 166.093  ops/s
  // FieldAccessBenchmark.optimizeHeapCaseClass  thrpt   15  14941.247 ± 325.665  ops/s
  // FieldAccessBenchmark.optimizeHeapMeta       thrpt   15   1771.349 ±  45.519  ops/s
  // FieldAccessBenchmark.optimizeHeapSpec       thrpt   15   1693.846 ± 135.487  ops/s

  // case class

  @Benchmark
  def caseClass(
    cs: CaseClassData
  ): IndexedSeq[(Boolean, Int, String, String)] =
    cs.foos.map(foo => (foo.b, foo.i, foo.s, foo.t))

  @Benchmark
  def caseClassSpec(
    cs: CaseClassData
  ): IndexedSeq[(Boolean, Int, String, String)] =
    cs.foosSpec.map(foo => (foo.a, foo.i, foo.s, foo.t))

  @Benchmark
  def caseClassMeta(
    cs: CaseClassData
  ): IndexedSeq[(Boolean, Int, String, String)] =
    cs.foosMeta.map(foo => (foo.a, foo.i, foo.s, foo.t))

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass(
    oh: OptimizeHeapData
  ): IndexedSeq[(Option[Boolean], Option[Boolean], Option[String])] =
    oh.foos.map(foo => (foo.a, foo.b, foo.s))

  @Benchmark
  def optimizeHeapSpec(
    oh: OptimizeHeapData
  ): IndexedSeq[(Option[Boolean], Option[Boolean], Option[String])] =
    oh.foosSpec.map(foo => (foo.a, foo.b, foo.s))

  @Benchmark
  def optimizeHeapMeta(
    oh: OptimizeHeapData
  ): IndexedSeq[(Option[Boolean], Option[Boolean], Option[String])] =
    oh.foosMeta.map(foo => (foo.a, foo.b, foo.s))
}
