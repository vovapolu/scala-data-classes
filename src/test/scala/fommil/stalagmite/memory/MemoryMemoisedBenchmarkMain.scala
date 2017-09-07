package fommil.stalagmite.memory

import _root_.scala._
import _root_.scala.Predef._
import fommil.stalagmite.data
import fommil.stalagmite._
import fommil.stalagmite.TestUtils._

import org.scalacheck._
import Arbitrary.arbitrary

import shapeless.tag.@@

import testing.weakmemoised

// CAUTION: It's recommended to run memory benchmarks in separate process

object MemoryMemoisedBenchmarkMain extends App {

  println("Running MemoryMemoisedBenchmarkMain")

  final case class Foo(b: Boolean, s: String)

  @data(
    memoise = true,
    memoiseStrong = true,
    memoiseEqualsByValue = false
  )
  class FooMeta(b: Boolean, s: String)

  @data(
    memoise = true,
    memoiseEqualsByValue = false
  )
  class FooMetaWeak(b: Boolean, s: String)

  @data(
    memoise = true,
    memoiseStrong = true,
    memoiseRefs = Seq('s),
    memoiseEqualsByValue = false
  )
  class FooMetaWithRefs(b: Boolean, s: String)

  val repeatingGenerator =
    Gen.listOfN(500000, arbitrary[(Boolean, String @@ SmallString)])
  val distinctGenerator =
    Gen.listOfN(500000, arbitrary[(Boolean, String @@ MediumString)])

  def generateRepeatingData =
    repeatingGenerator.sample.getOrElse(List())

  def generateDistinctData =
    distinctGenerator.sample.getOrElse(List())

  def mapData[T](data: () => Seq[(Boolean, String)],
                 mapF: (Boolean, String) => T): Seq[T] =
    data().map {
      case (a, b) => mapF(a, b)
    }

  for {
    (text, data) <- Seq(
                     ("---- Repeating data (good case for memosation) ----",
                      () => generateRepeatingData),
                     ("---- Distinct data (bad case for memosation) ----",
                      () => generateDistinctData)
                   )
  } {
    println(text)
    println()

    prettyPrintResults("Case class", measureMemoryConsumption() {
      mapData(data, Foo)
    })

    prettyPrintResults("Data class with strong memoisation",
                       measureMemoryConsumption(warmUpRepeats = 0) {
                         mapData(data, FooMeta.apply)
                       })
    prettyPrintResults("Strong memoisation with String caching",
                       measureMemoryConsumption(warmUpRepeats = 0) {
                         mapData(data, FooMetaWithRefs.apply)
                       })
    prettyPrintResults("Data class with weak memoisation",
                       measureMemoryConsumption(warmUpRepeats = 0) {
                         mapData(data, FooMetaWeak.apply)
                       })
    prettyPrintResults("Weak memoisation spec",
                       measureMemoryConsumption(warmUpRepeats = 0) {
                         mapData(data, weakmemoised.Foo.apply)
                       })
  }
}
