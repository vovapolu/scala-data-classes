package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import testing.caseclass
import testing.meta.FooMeta

class ShapelessBenchmark {
  import BenchmarkData._

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
