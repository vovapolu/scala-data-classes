package fommil.stalagmite.benchmarks

import testing.meta._
import testing.{ caseclass, memoised, optimiseheap }

import scala.util.Random

object BenchmarkData {
  case class FooCaseClass[T](b: Boolean, s: String, t: T, i: Int)
  case class FooOptimizeHeapCaseClass(a: Option[Boolean],
                                      b: Option[Boolean],
                                      s: Option[String])
  case class FooMemoisedCaseClass(b: Boolean, s: String)

  Random.setSeed(0xFEEL)

  val data: IndexedSeq[(Boolean, String, String, Int)] = {
    (1 to 10000).map(
      _ =>
        (Random.nextBoolean,
         Random.nextString(10),
         Random.nextString(20),
         Random.nextInt)
    )
  }
  val fooCaseClasses: IndexedSeq[FooCaseClass[String]] =
    data.map { case (a, b, c, d) => FooCaseClass(a, b, c, d) }
  val fooCaseClassesSpec: IndexedSeq[caseclass.Foo[String]] =
    data.map { case (a, b, c, d) => caseclass.Foo(a, b, c, d) }
  val fooCaseClassesMeta: IndexedSeq[FooMeta[String]] =
    data.map { case (a, b, c, d) => FooMeta(a, b, c, d) }

  val dataOptimizeHeap
    : IndexedSeq[(Option[Boolean], Option[Boolean], Option[String])] = {

    def nextOption[T](value: => T, someProb: Double = 0.5): Option[T] =
      if (Random.nextDouble < someProb) {
        Some(value)
      } else {
        None
      }

    (1 to 10000).map(
      _ =>
        (nextOption(Random.nextBoolean),
         nextOption(Random.nextBoolean),
         nextOption(Random.nextString(10)))
    )
  }
  val fooOptimizeHeapCaseClasses: IndexedSeq[FooOptimizeHeapCaseClass] =
    dataOptimizeHeap.map { case (a, b, c) => FooOptimizeHeapCaseClass(a, b, c) }
  val fooOptimizeHeapSpec: IndexedSeq[optimiseheap.Foo] =
    dataOptimizeHeap.map { case (a, b, c) => optimiseheap.Foo(a, b, c) }
  val fooOptimizeHeapMeta: IndexedSeq[FooMetaOptimiseHeap] =
    dataOptimizeHeap.map { case (a, b, c) => FooMetaOptimiseHeap(a, b, c) }

  val dataMemoised: IndexedSeq[(Boolean, String)] = {
    (1 to 10000).map(
      _ => (Random.nextBoolean, Random.nextString(10))
    )
  }

  val fooMemoisedCaseClasses: IndexedSeq[FooMemoisedCaseClass] =
    dataMemoised.map { case (a, b) => FooMemoisedCaseClass(a, b) }
  val fooMemoisedSpec: IndexedSeq[memoised.Foo] =
    dataMemoised.map { case (a, b) => memoised.Foo(a, b) }
  val fooMemoisedMeta: IndexedSeq[FooMetaMemoised] =
    dataMemoised.map { case (a, b) => FooMetaMemoised(a, b) }
}
