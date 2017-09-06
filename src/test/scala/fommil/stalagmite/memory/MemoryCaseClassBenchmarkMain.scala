package fommil.stalagmite.memory

import _root_.scala._
import _root_.scala.Predef._
import fommil.stalagmite.data
import fommil.stalagmite.TestUtils._

import org.scalacheck._
import Arbitrary.arbitrary

import org.scalacheck.rng.Seed
import shapeless.tag.@@

// CAUTION: It's recommended to run memory benchmarks in separate process

object MemoryCaseClassBenchmarkMain extends App {

  println("Running MemoryCaseClassBenchmark")

  final case class Foo(i: Int, b: Boolean, s: String)

  @data class FooMeta(i: Int, b: Boolean, s: String)

  val generator =
    Gen.listOfN(500000, arbitrary[(Int, Boolean, String @@ MediumString)])

  def generateData =
    generator(Gen.Parameters.default, Seed(0xBABE1)).getOrElse(List.empty)

  prettyPrintResults("Case class", measureMemoryConsumption() {
    generateData.map {
      case (a, b, c) => Foo(a, b, c)
    }
  })

  prettyPrintResults("Data class", measureMemoryConsumption() {
    generateData.map {
      case (a, b, c) => FooMeta(a, b, c)
    }
  })
}
