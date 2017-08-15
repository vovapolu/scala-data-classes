package fommil.stalagmite.benchmarks

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  ObjectInputStream,
  ObjectOutputStream
}

import org.openjdk.jmh.annotations.Benchmark
import testing.meta._
import testing.{ caseclass, memoised, optimiseheap, weakmemoised }

class SerializationBenchmark {

  import BenchmarkData._

  // jmh:run -i 15 -wi 15 -f1 -t10 .*SerializationBenchmark
  // Benchmark                                      Mode  Cnt    Score    Error  Units
  // SerializationBenchmark.caseClass              thrpt   15   88.145 ± 11.556  ops/s
  // SerializationBenchmark.caseClassMeta          thrpt   15   89.039 ± 15.152  ops/s
  // SerializationBenchmark.caseClassSpec          thrpt   15   63.188 ± 10.900  ops/s
  // SerializationBenchmark.memoisedCaseClass      thrpt   15  113.520 ± 10.755  ops/s
  // SerializationBenchmark.memoisedMeta           thrpt   15  116.444 ± 27.910  ops/s
  // SerializationBenchmark.memoisedSpec           thrpt   15  108.972 ±  4.081  ops/s
  // SerializationBenchmark.memoisedWeak           thrpt   15  131.400 ± 16.620  ops/s
  // SerializationBenchmark.optimizeHeapCaseClass  thrpt   15   60.627 ± 11.772  ops/s
  // SerializationBenchmark.optimizeHeapMeta       thrpt   15   46.490 ±  5.667  ops/s
  // SerializationBenchmark.optimizeHeapSpec       thrpt   15   44.397 ±  6.318  ops/s

  def readWriteClasses[T](classes: IndexedSeq[T]): IndexedSeq[T] = {
    val bytes_out = new ByteArrayOutputStream
    val out       = new ObjectOutputStream(bytes_out)
    classes.foreach(out.writeObject)
    val bytes_in = new ByteArrayInputStream(bytes_out.toByteArray)
    val in       = new ObjectInputStream(bytes_in)
    (1 to classes.length).map(_ => in.readObject().asInstanceOf[T])
  }

  // case class

  @Benchmark
  def caseClass(cs: CaseClassData): IndexedSeq[FooCaseClass[String]] =
    readWriteClasses(cs.foos)

  @Benchmark
  def caseClassSpec(cs: CaseClassData): IndexedSeq[caseclass.Foo[String]] =
    readWriteClasses(cs.foosSpec)

  @Benchmark
  def caseClassMeta(cs: CaseClassData): IndexedSeq[FooMeta[String]] =
    readWriteClasses(cs.foosMeta)

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass(
    oh: OptimizeHeapData
  ): IndexedSeq[FooOptimizeHeapCaseClass] =
    readWriteClasses(oh.foos)

  @Benchmark
  def optimizeHeapSpec(oh: OptimizeHeapData): IndexedSeq[optimiseheap.Foo] =
    readWriteClasses(oh.foosSpec)

  @Benchmark
  def optimizeHeapMeta(oh: OptimizeHeapData): IndexedSeq[FooMetaOptimiseHeap] =
    readWriteClasses(oh.foosMeta)

  // memoised

  @Benchmark
  def memoisedCaseClass(m: MemoisedData): IndexedSeq[FooMemoisedCaseClass] =
    readWriteClasses(m.foos)

  @Benchmark
  def memoisedSpec(m: MemoisedData): IndexedSeq[memoised.Foo] =
    readWriteClasses(m.foosSpec)

  @Benchmark
  def memoisedMeta(m: MemoisedData): IndexedSeq[FooMetaMemoised] =
    readWriteClasses(m.foosMeta)

  @Benchmark
  def memoisedWeakSpec(m: MemoisedData): IndexedSeq[weakmemoised.Foo] =
    readWriteClasses(m.foosWeakSpec)
}
