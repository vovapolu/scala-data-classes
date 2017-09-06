package fommil.stalagmite.memory

import fommil.stalagmite.TestUtils.MediumString

import _root_.scala._
import _root_.scala.Predef._
import fommil.stalagmite.data
import fommil.stalagmite.TestUtils._

import org.scalacheck._
import Arbitrary.arbitrary

import org.scalacheck.rng.Seed
import shapeless.tag.@@

// CAUTION: It's recommended to run memory benchmarks in separate process

object MemoryOptimizeHeapBenchmarkMain extends App {

  println("Running MemoryOptimizeHeapBenchmarkMain")

  final case class Foo(i: Option[Int],
                       s: Option[String],
                       b1: Option[Boolean],
                       b2: Option[Boolean],
                       b3: Option[Boolean],
                       b4: Option[Boolean])

  @data(
    optimiseHeapOptions = true,
    optimiseHeapStrings = true,
    optimiseHeapBooleans = true
  ) class FooMeta(i: Option[Int],
                  s: Option[String],
                  b1: Option[Boolean],
                  b2: Option[Boolean],
                  b3: Option[Boolean],
                  b4: Option[Boolean])

  val generator =
    Gen.listOfN(500000,
                arbitrary[
                  (Option[Int],
                   Option[String @@ MediumString],
                   Option[Boolean],
                   Option[Boolean],
                   Option[Boolean],
                   Option[Boolean])
                ])

  def generateData =
    generator(Gen.Parameters.default, Seed(0xBABE2)).getOrElse(List.empty)

  prettyPrintResults(
    "Data class",
    measureMemoryConsumption() {
      generateData.map {
        case (a, b, c, d, e, f) => FooMeta(a, b, c, d, e, f)
      }
    }
  )
  prettyPrintResults(
    "Case class",
    measureMemoryConsumption() {
      generateData.map {
        case (a, b, c, d, e, f) => Foo(a, b, c, d, e, f)
      }
    }
  )
}
