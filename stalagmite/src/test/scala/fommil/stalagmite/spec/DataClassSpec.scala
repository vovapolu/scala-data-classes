package fommil.stalagmite.spec

import _root_.scala._
import _root_.scala.Predef._

import org.scalatest._
import org.scalatest.Matchers._

import fommil.stalagmite.data

class DataClassSpec extends FlatSpec with ParallelTestExecution {
  "@data class" should
    "be defined without methods, modifiers and var params" in {
    """@data class A(a: Int)""" should compile
    """@data class A(a: Int = 1)""" should compile
    """@data class A(a: Int = 1, b: Int)""" should compile

    """@data class A""" shouldNot compile

    """@data case class A(a: Int)""" shouldNot compile
    """@data final class A(a: Int)""" shouldNot compile
    """@data abstract class A(a: Int)""" shouldNot compile

    """@data object A(a: Int)""" shouldNot compile
    """@data class A(a: Int); object A {}""" shouldNot compile

    """@data class A(a: Int) {} """ should compile
    """@data class A(a: Int) { def hello = "world" }""" shouldNot compile

    """@data class A(var a: Int)""" shouldNot compile
    """@data class A(override a: Int)""" shouldNot compile
    """@data class A(lazy a: Int)""" shouldNot compile
    """@data class A(private a: Int)""" shouldNot compile

    """@data class A(a: Int)(b: Int)""" shouldNot compile
    """@data class A private (a: Int)""" shouldNot compile

    abstract class B {
      val b: Int
    }
    """@data class A(a: Int) extends B""" shouldNot compile
    //"""@data class A(a: Int) extends { val b = 1 } with B""" shouldNot compile
    """@data class A(a: Int) { notThis => }""" shouldNot compile
  }

  it should "have correct field types" in {
    """@data class A(a: Int)""" should compile
    """@data class A(a: Option[Int])""" should compile
    """@data class A(a: Option[Either[String, List[Int]]])""" should compile
    """@data class A[X, Y](a: Option[Either[X, List[Y]]])""" should compile

    """@data class A(a: => Int)""" shouldNot compile
    """@data class A(a: Int*)""" shouldNot compile
  }
}
