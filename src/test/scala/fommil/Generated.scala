package fommil

import fommil.data.impl.DataImpl
import org.scalatest._
import org.scalatest.Matchers._

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

  def checkGenFile(filename: String) = {
    val source = io.Source.fromURL(getClass.getResource(s"/generatedTests/$filename.scala")).mkString
    source.split("//---") match {
      case Array(input, target) =>
        val inputMods = input.lines.take(1).toList.head.stripPrefix("//").split(" ")
        val inputTree = input.lines.drop(1).mkString.parse[Stat].get match {
          case clazz: Defn.Class => clazz
          case _                 => fail("Input should be a single class")
        }
        val targetTree = target.parse[Stat].get
        assertStructurallyEqual(DataImpl.expand(inputTree, inputMods.map(mod => mod -> true).toMap), targetTree)
      case _ => fail("Source has two or more delimiters \"---\"")
    }
  }

  "@data-generated class without mods" should "have only basic methods" in {
    checkGenFile("NoModsGen")
  }

  "@data-generated class" should "have case-class methods" in {
    checkGenFile("CaseClassParityGen")
    checkGenFile("CaseClassTypedParityGen")
  }
}
