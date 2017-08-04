package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

class ProductElementBenchmark {
  import BenchmarkData._

  // case class

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
