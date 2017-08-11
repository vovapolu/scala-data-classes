package fommil.stalagmite

import _root_.scala._
import _root_.scala.Predef._
import scala.util.Random
import java.lang.Runtime

import com.google.common.testing.GcFinalization

object TestUtils {

  def generateWithDuplicates[T](generator: Unit => T,
                                genCount: Int,
                                duplicatesCount: Int): IndexedSeq[T] = {
    val data = (1 to genCount).map(_ => generator(()))
    data ++ (1 to duplicatesCount).map(_ => data(Random.nextInt(data.length)))
  }

  def randomNextOption[T](value: => T, someProb: Double = 0.5): Option[T] =
    if (Random.nextDouble < someProb) {
      Some(value)
    } else {
      None
    }

  def measureMemoryConsumption[T](name: String,
                                  repeats: Int = 5)(u: => T): Unit = {
    println(name)
    val rt            = Runtime.getRuntime
    GcFinalization.awaitFullGc()
    val initialMemory = rt.totalMemory - rt.freeMemory
    val ms = for (i <- 1 to repeats)
      yield {
        GcFinalization.awaitFullGc()
        val startMemory = rt.totalMemory - rt.freeMemory
        val t           = u
        GcFinalization.awaitFullGc()
        val finishMemory = rt.totalMemory - rt.freeMemory

        val consumedMemory = finishMemory - startMemory
        val totallMemory   = finishMemory - initialMemory
        println(
          s"Iteration $i: consumed ${consumedMemory / 1024} kb, " +
            s"totally ${totallMemory / 1024} kb"
        )
        consumedMemory
      }

    val mean = ms.sum / ms.length
    val std  = math.pow(ms.map(m => (m - mean) * (m - mean)).sum.toDouble, 0.5)

    println(f"""
               |$name:
               | mean - ${mean / 1024}%d kb
               | std  - ${std / 1024}%2.2f kb
       """.stripMargin)
  }
}
