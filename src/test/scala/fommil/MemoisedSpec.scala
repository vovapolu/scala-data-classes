package fommil

import java.io._

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import org.slf4j.LoggerFactory
import org.slf4j.bridge.SLF4JBridgeHandler
import testing.memoised._
import shapeless._

class MemoisedSpec extends FlatSpec {
  SLF4JBridgeHandler.removeHandlersForRootLogger()
  SLF4JBridgeHandler.install()
  val log = LoggerFactory.getLogger(this.getClass)

  val foo = Foo(true, "hello")

  "@data(memoised) class Foo" should "have equals, hashCode and toString defined" in {
    foo.hashCode shouldBe 1289111417
    foo should equal(foo)
    foo should be theSameInstanceAs (Foo(true, "hello"))
    foo should not equal (Foo(false, "hello"))
    foo.toString should equal("Foo(true,hello)")
    Foo.toString should equal("Foo")
  }

  it should "not expose its constructor" in {
    """new Foo(true, "hello")""" shouldNot compile

    """new Foo()""" shouldNot compile
  }

  it should "expose its fields" in {
    foo.a should equal(true)
    foo.s should equal("hello")
  }

  it should "have a copy method" in {
    foo.copy(a = false) should equal(Foo(false, "hello"))
    foo.copy(s = "foo") should equal(Foo(true, "foo"))
  }

  it should "have a pattern matcher" in {
    foo should matchPattern { case Foo(true, "hello") => }
  }

  it should "be serialisable (with memoisation holding)" in {
    val bytes_out = new ByteArrayOutputStream
    val out = new ObjectOutputStream(bytes_out)

    out.writeObject(foo)
    out.close()

    val bytes_in = new ByteArrayInputStream(bytes_out.toByteArray())
    val in = new ObjectInputStream(bytes_in)

    val recovered = in.readObject().asInstanceOf[Foo]

    recovered should equal(foo)
    recovered should be theSameInstanceAs (foo)
  }

  /*
  it should "have a Generic" in {
    implicit val G: Generic[Foo] = cachedImplicit
    import G._

    from(to(foo)) should equal(foo)
    from(to(foo)) should be theSameInstanceAs (foo)
  }

  it should "have a LabelledGeneric" in {
    implicit val LG: LabelledGeneric[Foo] = cachedImplicit
    import LG._

    from(to(foo)) should equal(foo)
    from(to(foo)) should be theSameInstanceAs (foo)
  }

  it should "have a Typeable" in {
    val T = Typeable[Foo]

    T.describe should equal("Foo[Boolean,String]")

    T.cast("hello") shouldBe empty
    T.cast(1L) shouldBe empty
    T.cast(foo).value shouldBe foo
    T.cast(foo).value should be theSameInstanceAs (foo)
  }

  it should "allow user-land Show (Generic) derivation" in {
    import cats.Semigroup
    import cats.implicits._
    import cats.derived.semigroup._
    import cats.derived.semigroup.legacy._

    implicit val B: Semigroup[Boolean] = new Semigroup[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x & y
    }

    implicit val S: Semigroup[Foo] = cachedImplicit

    S.combine(foo, foo) should equal(Foo(true, "hellohello"))
  }

  // redundant, just using it becuase I am familiar with the required imports
  it should "allow user-land JsonFormat (LabelledGeneric) derivation" in {
    import spray.json._
    import fommil.sjs.FamilyFormats._

    implicit val J: JsonFormat[Foo] = cachedImplicit
  }

  it should "have memoised string fields" in {
    fail
  }
   */
}
