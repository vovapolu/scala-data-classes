package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import testing.caseclass
import testing.meta.FooMeta

class ShapelessBenchmark {
  import BenchmarkData._

  // jmh:run -i 15 -wi 15 -f1 -t10 .*ShapelessBenchmark
  // Benchmark                          Mode  Cnt     Score     Error  Units
  // ShapelessBenchmark.caseClass      thrpt   15  3846.453 ±  67.180  ops/s
  // ShapelessBenchmark.caseClassMeta  thrpt   15  1790.079 ± 539.833  ops/s
  // ShapelessBenchmark.caseClassSpec  thrpt   15  1654.259 ± 389.458  ops/s

  // case class

  @Benchmark
  def caseClass(cs: CaseClassData): IndexedSeq[FooCaseClass[String]] = {
    import G._
    cs.foos.map(foo => from(to(foo)))
  }

  @Benchmark
  def caseClassSpec(cs: CaseClassData): IndexedSeq[caseclass.Foo[String]] = {
    import GSpec._
    cs.foosSpec.map(foo => from(to(foo)))
  }

  @Benchmark
  def caseClassMeta(cs: CaseClassData): IndexedSeq[FooMeta[String]] = {
    import GMeta._
    cs.foosMeta.map(foo => from(to(foo)))
  }

}
