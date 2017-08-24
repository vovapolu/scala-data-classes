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
