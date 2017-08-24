package fommil.stalagmite.spec

import _root_.scala._
import _root_.scala.Predef._
import fommil.stalagmite.{ DataImpl, DataMods }
import org.scalatest.Matchers._
import org.scalatest._

import scala.collection.immutable.Seq
import scala.io
import scala.meta._
import scala.meta.testkit._

class Generated extends FlatSpec with ParallelTestExecution {

  def assertStructurallyEqual(obtained: Tree, expected: Tree): Unit =
    StructurallyEqual(obtained, expected) match {
      case Left(AnyDiff(x, y)) =>
        fail(s"""Not Structurally equal!:
                |obtained: $x
                |expected: $y
             """.stripMargin)
      case _ =>
    }

  def checkGenFile(filename: String, printStructure: Boolean = false) = {
    val source = io.Source
      .fromURL(getClass.getResource(s"/generatedTests/$filename.scala"))
      .mkString
    source.split("//---") match {
      case Array(input, target) =>
        val inputMods = input.lines
          .takeWhile(_.startsWith("//"))
          .toList
        val booleanMods = inputMods.headOption
          .getOrElse("")
          .stripPrefix("//")
          .split(" ")
          .toList
        val extraMods = inputMods
          .drop(1)
          .map(str => Seq(str.stripPrefix("//").split(" "): _*))
          .filter(_.nonEmpty)
          .map { case mod :: keys => mod -> Right(keys) }

        val inputTree = input.lines
          .dropWhile(_.startsWith("//"))
          .mkString
          .parse[Stat]
          .get match {
          case clazz: Defn.Class => clazz
          case _                 => fail("Input should be a single class")
        }
        val targetTree = target.parse[Stat].get
        val expandedTree = DataImpl.expand(
          inputTree,
          DataMods.fromPairs(booleanMods.map(_ -> Left(true)) ++ extraMods,
                             applyDefaults = false)
        )
        if (printStructure)
          println(expandedTree)
        assertStructurallyEqual(expandedTree, targetTree)
      case _ => fail("Source has two or more delimiters \"---\"")
    }
  }

  "@data-generated class without mods" should "have only basic methods" in {
    checkGenFile("NoModsGen")
  }

  "@data-generated class with only one field" should
    "have correct definition" in {
    checkGenFile("OneFieldGen")
  }

  "@data-generated class" should "have case-class methods" in {
    checkGenFile("CaseClassParityGen")
    checkGenFile("CaseClassTypedParityGen")
  }

  "@data-generated class with memoising" should
    "have cache, corresponding apply method" in {
    checkGenFile("MemoisedGen")
    checkGenFile("StronglyMemoisedGen")
  }

  "@data-generated class with lazy chaching of .hashCode and .toString" should
    "have lazy modifiers" in {
    checkGenFile("MemoisedLazyGen")
  }

  "@data-generated class with heap optimization" should
    "have pack method and needed logic in apply, readObject and getters" in {
    checkGenFile("HeapOptimizationGen")
  }
}
