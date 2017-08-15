package fommil.stalagmite

import _root_.scala._
import _root_.scala.Predef._
import java.lang.Runtime

import com.google.common.testing.GcFinalization
import org.scalacheck.{ Arbitrary, Gen }
import shapeless.tag
import shapeless.tag.@@

import scala.reflect.ClassTag

object TestUtils {

  def measureMemoryConsumption[T](
    repeats: Int = 5
  )(u: => T): Seq[(Long, Long)] = {
    val rt = Runtime.getRuntime
    GcFinalization.awaitFullGc()
    val initialMemory = rt.totalMemory - rt.freeMemory
    for (i <- 1 to repeats)
      yield {
        GcFinalization.awaitFullGc()
        val startMemory = rt.totalMemory - rt.freeMemory
        val t           = u
        GcFinalization.awaitFullGc()
        val finishMemory = rt.totalMemory - rt.freeMemory

        (finishMemory - startMemory, finishMemory - initialMemory)
      }
  }

  def prettyPrintResults(name: String,
                         memoryMeasurements: Seq[(Long, Long)]): Unit = {
    println(name)
    memoryMeasurements.zipWithIndex.foreach {
      case ((consumedMemory, totalMemory), i) =>
        println(
          s"Iteration $i: consumed ${consumedMemory / 1024} kb, " +
            s"totally ${totalMemory / 1024} kb"
        )
    }

    val (consumed, _) = memoryMeasurements.unzip

    val mean = consumed.sum / consumed.length
    val std =
      math.pow(consumed.map(m => (m - mean) * (m - mean)).sum.toDouble, 0.5)

    println(f"""
               |$name:
               | mean - ${mean / 1024}%d kb
               | std  - ${std / 1024}%2.2f kb
       """.stripMargin)
  }

  trait SmallString
  trait MediumString
  trait LargeString

  implicit lazy val arbSmallString: Arbitrary[String @@ SmallString] =
    Arbitrary(
      Gen
        .listOfN(2, Gen.alphaNumChar)
        .map(l => tag[SmallString][String](l.mkString))
    )
  implicit lazy val arbMediumString: Arbitrary[String @@ MediumString] =
    Arbitrary(
      Gen
        .listOfN(10, Gen.alphaNumChar)
        .map(l => tag[MediumString][String](l.mkString))
    )
  implicit lazy val arbLargeString: Arbitrary[String @@ LargeString] =
    Arbitrary(
      Gen
        .listOfN(50, Gen.alphaNumChar)
        .map(l => tag[LargeString][String](l.mkString))
    )

  def vectorWithDuplicates[T: ClassTag](gen: Gen[T],
                                        genCount: Int,
                                        duplicatesCount: Int): Gen[Vector[T]] =
    for {
      data          <- Gen.listOfN(genCount, gen).map(_.toVector)
      duplicateIdxs <- Gen.listOfN(duplicatesCount, Gen.choose(0, genCount - 1))
    } yield {
      data ++ duplicateIdxs.map(data)
    }
}
