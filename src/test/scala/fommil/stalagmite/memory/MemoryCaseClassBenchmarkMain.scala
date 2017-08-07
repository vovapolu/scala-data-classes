package fommil.stalagmite.memory

import _root_.scala._
import _root_.scala.Predef._
import fommil.stalagmite.{ data, TestUtils }

import scala.util.Random

object MemoryCaseClassBenchmarkMain extends App {

  println("Running MemoryCaseClassBenchmark")

  final case class Foo(i: Int, b: Boolean, s: String)

  @data class FooMeta(i: Int, b: Boolean, s: String)

  def generateData = (1 to 1000000).map(
    _ =>
      (
        Random.nextInt(1000),
        Random.nextBoolean(),
        Random.nextString(5),
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
}
