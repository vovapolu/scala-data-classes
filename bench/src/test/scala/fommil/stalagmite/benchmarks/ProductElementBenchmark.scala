package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark

class ProductElementBenchmark {
  import BenchmarkData._

  // case class

  @Benchmark
  def caseClass: IndexedSeq[(Any, Any, Any, Any)] =
    fooCaseClasses.map(
      foo =>
        (foo.productElement(0),
         foo.productElement(1),
         foo.productElement(2),
         foo.productElement(3))
    )

  @Benchmark
  def caseClassSpec: IndexedSeq[(Any, Any, Any, Any)] =
    fooCaseClassesSpec.map(
      foo =>
        (foo.productElement(0),
         foo.productElement(1),
         foo.productElement(2),
         foo.productElement(3))
    )

  @Benchmark
  def caseClassMeta: IndexedSeq[(Any, Any, Any, Any)] =
    fooCaseClassesMeta.map(
      foo =>
        (foo.productElement(0),
         foo.productElement(1),
         foo.productElement(2),
         foo.productElement(3))
    )

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass: IndexedSeq[(Any, Any, Any)] =
    fooOptimizeHeapCaseClasses.map(
      foo =>
        (foo.productElement(0), foo.productElement(1), foo.productElement(2))
    )

  @Benchmark
  def optimizeHeapMeta: IndexedSeq[(Any, Any, Any)] =
    fooOptimizeHeapMeta.map(
      foo =>
        (foo.productElement(0), foo.productElement(1), foo.productElement(2))
    )
}
