package fommil.stalagmite.memory

import fommil.stalagmite.{ data, TestUtils }

import scala.util.Random

object MemoryMemoisedBenchmarkMain extends App {
  println("Running MemoryMemoisedBenchmarkMain")

  final case class Foo(i: Int, b: Boolean, s: String)

  @data(
    memoise = true,
    memoiseStrong = true
  )
  class FooMeta(i: Int, b: Boolean, s: String)

  @data(
    memoise = true,
    memoiseStrong = true,
    memoiseRefs = Seq('s)
  )
  class FooMetaWithRefs(i: Int, b: Boolean, s: String)

  def generateData = (1 to 1000000).map(
    _ =>
      (
        Random.nextInt(10),
        Random.nextBoolean(),
        Random.nextString(1),
    )
  )

  val foosMemory = TestUtils.measureMemoryConsumption("Case class") {
    generateData.map {
      case (a, b, c) => Foo(a, b, c)
    }
  }
  val foosMetaMemory = TestUtils.measureMemoryConsumption("Data class") {
    generateData.map {
      case (a, b, c) => FooMeta(a, b, c)
    }
  }
  val foosMetaWithRefsMemory =
    TestUtils.measureMemoryConsumption("Data class with refs") {
      generateData.map {
        case (a, b, c) => FooMetaWithRefs(a, b, c)
      }
    }
}
