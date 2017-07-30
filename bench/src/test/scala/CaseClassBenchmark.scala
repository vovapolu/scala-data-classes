package fommil.stalagmite.benchmarks

import org.openjdk.jmh.annotations.Benchmark
import testing.caseclass.Foo
import testing.meta.FooMeta

object CaseClassBenchmark {
  case class FooCaseClass[T](b: Boolean, s: String, t: T, i: Int)
}

class CaseClassBenchmark {

  import CaseClassBenchmark._

  @Benchmark
  def createCaseClass: FooCaseClass[String] =
    FooCaseClass(true, "hello", "world", 1)

  @Benchmark
  def createCaseClassSpec: Foo[String] =
    Foo(true, "hello", "world", 1)

  @Benchmark
  def createMetaCaseClass: FooMeta[String] =
    FooMeta(true, "hello", "world", 1)
}
