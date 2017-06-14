package fommil

import fommil.dataMacro.DataMacro
import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

import scala.io
import scala.meta._
import scala.meta.testkit._

class Generated extends FlatSpec with ParallelTestExecution {

  def assertStructurallyEqual(obtained: Tree, expected: Tree): Unit = {
    StructurallyEqual(obtained, expected) match {
      case Left(AnyDiff(x, y)) =>
        fail(s"""Not Structurally equal!:
                |obtained: $x
                |expected: $y
             """.stripMargin)
      case _ =>
    }
  }

  "@data-generated class" should "have case-class methods" in {
    val source = io.Source.fromURL(getClass.getResource("/generatedTests/CaseClassParityGen.scala")).mkString
    source.split("---") match {
      case Array(input, target) =>
        val inputTree = input.parse[Stat].get match {
          case clazz: Defn.Class => clazz
          case _                 => fail("Input should be a single class")
        }
        val targetTree = target.parse[Stat].get
        assertStructurallyEqual(DataMacro.expand(inputTree), targetTree)
      case _ => fail("Source has two or more delimiters \"---\"")
    }
  }
}
