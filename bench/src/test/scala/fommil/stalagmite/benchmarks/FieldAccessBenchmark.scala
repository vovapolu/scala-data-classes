package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

class FieldAccessBenchmark {
  import BenchmarkData._

  // case class

  @Benchmark
  def caseClass: IndexedSeq[(Boolean, Int, String, String)] =
    fooCaseClasses.map(foo => (foo.b, foo.i, foo.s, foo.t))

  @Benchmark
  def caseClassSpec: IndexedSeq[(Boolean, Int, String, String)] =
    fooCaseClassesSpec.map(foo => (foo.a, foo.i, foo.s, foo.t))

  @Benchmark
  def caseClassMeta: IndexedSeq[(Boolean, Int, String, String)] =
    fooCaseClassesMeta.map(foo => (foo.a, foo.i, foo.s, foo.t))

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass
    : IndexedSeq[(Option[Boolean], Option[Boolean], Option[String])] =
    fooOptimizeHeapCaseClasses.map(foo => (foo.a, foo.b, foo.s))

  @Benchmark
  def optimizeHeapSpec
    : IndexedSeq[(Option[Boolean], Option[Boolean], Option[String])] =
    fooOptimizeHeapSpec.map(foo => (foo.a, foo.b, foo.s))

  @Benchmark
  def optimizeHeapMeta
    : IndexedSeq[(Option[Boolean], Option[Boolean], Option[String])] =
    fooOptimizeHeapMeta.map(foo => (foo.a, foo.b, foo.s))
}
