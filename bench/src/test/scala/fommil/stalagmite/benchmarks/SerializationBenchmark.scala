package fommil.stalagmite.benchmarks

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  ObjectInputStream,
  ObjectOutputStream
}

import org.openjdk.jmh.annotations.Benchmark
import testing.meta._
import testing.{ caseclass, memoised, optimiseheap }

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
  def caseClass: IndexedSeq[FooCaseClass[String]] =
    readWriteClasses(fooCaseClasses)

  @Benchmark
  def caseClassSpec: IndexedSeq[caseclass.Foo[String]] =
    readWriteClasses(fooCaseClassesSpec)

  @Benchmark
  def caseClassMeta: IndexedSeq[FooMeta[String]] =
    readWriteClasses(fooCaseClassesMeta)

  // optimize heap

  @Benchmark
  def optimizeHeapCaseClass: IndexedSeq[FooOptimizeHeapCaseClass] =
    readWriteClasses(fooOptimizeHeapCaseClasses)

  @Benchmark
  def optimizeHeapSpec: IndexedSeq[optimiseheap.Foo] =
    readWriteClasses(fooOptimizeHeapSpec)

  @Benchmark
  def optimizeHeapMeta: IndexedSeq[FooMetaOptimiseHeap] =
    readWriteClasses(fooOptimizeHeapMeta)

  // memoised

  @Benchmark
  def memoisedCaseClass: IndexedSeq[FooMemoisedCaseClass] =
    readWriteClasses(fooMemoisedCaseClasses)

  @Benchmark
  def memoisedSpec: IndexedSeq[memoised.Foo] =
    readWriteClasses(fooMemoisedSpec)

  @Benchmark
  def memoisedMeta: IndexedSeq[FooMetaMemoised] =
    readWriteClasses(fooMemoisedMeta)
}
