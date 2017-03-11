package fommil

import java.io._

import org.scalatest._
import org.scalatest.Matchers._
import testing.caseclass._

class CaseClassParitySpec extends FlatSpec {
  // should behave like:
  //
  // final case class Foo[+T] private (a: Boolean, s: String, t: T, i: Int = 0)

  val foo = Foo(true, "hello", "world", 1)

  "@data(product) Foo[+]" should "have equals, hashCode and toString defined" in {
    foo.hashCode shouldBe -1034845328
    foo should equal(foo)
    foo should not be theSameInstanceAs(Foo(true, "hello", "world", 1))
    foo should not equal (Foo(false, "hello", "world", 1))
    foo.toString should equal("Foo(true,hello,world,1)")
  }

  it should "not expose its constructor" in {
    """new Foo(true, "hello", "world", 1)""" shouldNot compile

    """new Foo(true, "hello", "world")""" shouldNot compile

    """new Foo()""" shouldNot compile
  }

  it should "expose its fields" in {
    foo.a should equal(true)
    foo.s should equal("hello")
    foo.t should equal("world")
    foo.i should equal(1)
  }

  it should "have default parameters" in {
    Foo(false, "goodbye", 13) should equal(Foo(false, "goodbye", 13, 0))
  }

  it should "have a copy method" in {
    foo.copy(a = false) should equal(Foo(false, "hello", "world", 1))
    foo.copy(s = "foo") should equal(Foo(true, "foo", "world", 1))
    foo.copy(t = 13) should equal(Foo(true, "hello", 13, 1))
    foo.copy(i = -1) should equal(Foo(true, "hello", "world", -1))
  }

  it should "have a pattern matcher" in {
    foo should matchPattern { case Foo(true, "hello", "world", 1) => }
  }

  it should "implement Product" in {
    foo.productIterator.toList should contain theSameElementsInOrderAs (List(true, "hello", "world", 1))
  }

  it should "be serialisable" in {
    val bytes_out = new ByteArrayOutputStream
    val out = new ObjectOutputStream(bytes_out)

    out.writeObject(foo)
    out.close()

    val bytes_in = new ByteArrayInputStream(bytes_out.toByteArray())
    val in = new ObjectInputStream(bytes_in)

    val recovered = in.readObject().asInstanceOf[Foo[String]]

    recovered should equal(foo)
    recovered should not be theSameInstanceAs(foo)
  }

  it should "have a LabelledGeneric" in {
    import shapeless._
    import shapeless._
    import shapeless.syntax.singleton._

    implicit val generic: LabelledGeneric[Foo[String]] = cachedImplicit

    val repr = ('a ->> true) :: ('s ->> "hello") :: ('t ->> "world") :: ('i ->> 1) :: HNil

    generic.to(foo) should equal(repr)
    //generic.from(repr) should equal(foo)
    generic.from(generic.to(foo)) should equal(foo)
  }

}
