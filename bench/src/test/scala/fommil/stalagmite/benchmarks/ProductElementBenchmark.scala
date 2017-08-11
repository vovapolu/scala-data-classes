package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

class ProductElementBenchmark {
  import BenchmarkData._

  // case class

  // jmh:run -i 15 -wi 15 -f1 -t10 .*ProductElementBenchmark
  // Benchmark                                       Mode  Cnt      Score     Error  Units
  // ProductElementBenchmark.caseClass              thrpt   15   7576.747 ±  86.005  ops/s
  // ProductElementBenchmark.caseClassMeta          thrpt   15   7029.297 ± 849.618  ops/s
  // ProductElementBenchmark.caseClassSpec          thrpt   15   7253.255 ± 236.485  ops/s
  // ProductElementBenchmark.optimizeHeapCaseClass  thrpt   15  15081.511 ± 322.709  ops/s
  // ProductElementBenchmark.optimizeHeapMeta       thrpt   15   1798.293 ±  80.164  ops/s
  @Benchmark
  def caseClass(cs: CaseClassData): IndexedSeq[(Any, Any, Any, Any)] =
    cs.foos.map(
      foo =>
        (foo.productElement(0),
         foo.productElement(1),
         foo.productElement(2),
         foo.productElement(3))
    )

  @Benchmark
  def caseClassSpec(cs: CaseClassData): IndexedSeq[(Any, Any, Any, Any)] =
    cs.foosSpec.map(
      foo =>
        (foo.productElement(0),
         foo.productElement(1),
         foo.productElement(2),
         foo.productElement(3))
    )

  @Benchmark
  def caseClassMeta(cs: CaseClassData): IndexedSeq[(Any, Any, Any, Any)] =
    cs.foosMeta.map(
      foo =>
        (foo.productElement(0),
         foo.productElement(1),
         foo.productElement(2),
         foo.productElement(3))
    )

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass(oh: OptimizeHeapData): IndexedSeq[(Any, Any, Any)] =
    oh.foos.map(
      foo =>
        (foo.productElement(0), foo.productElement(1), foo.productElement(2))
    )

  @Benchmark
  def optimizeHeapMeta(oh: OptimizeHeapData): IndexedSeq[(Any, Any, Any)] =
    oh.foosMeta.map(
      foo =>
        (foo.productElement(0), foo.productElement(1), foo.productElement(2))
    )
}
