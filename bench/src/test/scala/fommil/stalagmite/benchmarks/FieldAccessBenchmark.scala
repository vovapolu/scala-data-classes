package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

class FieldAccessBenchmark {
  import BenchmarkData._

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
