package fommil

import java.io._

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import testing.caseclass._
import testing.meta._
import shapeless._

class CaseClassParitySpec extends FlatSpec with ParallelTestExecution {
  // should behave like:
  //
  // final case class Foo[+T] private (a: Boolean, s: String, t: T, i: Int = 0)

  val foo = Foo(true, "hello", "world", 1)
  val metaFoo = FooMetaCaseClass(true, "hello", "world", 1)

  "@data(product) class Foo[+]" should "have equals, hashCode and toString defined" in {
    foo.hashCode shouldBe metaFoo.hashCode
    foo should equal(foo)
    foo should not be theSameInstanceAs(Foo(true, "hello", "world", 1))
    foo should not equal (Foo(false, "hello", "world", 1))
    foo.toString should equal("Foo(true,hello,world,1)")
    Foo.toString should equal("Foo")
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
    foo.productPrefix should equal("Foo")
    foo.productIterator.toList should contain theSameElementsInOrderAs (List(true, "hello", "world", 1))
  }

  it should "be serialisable" in {
    // should really check with the serialised form of the equivalent case class

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

  it should "have a Generic" in {
    implicit val G: Generic[Foo[String]] = cachedImplicit
    import G._

    from(to(foo)) should equal(foo)
  }

  it should "have a LabelledGeneric" in {
    implicit val LG: LabelledGeneric[Foo[String]] = cachedImplicit
    import LG._

    from(to(foo)) should equal(foo)
  }

  it should "have a Typeable" in {
    val T = Typeable[Foo[Long]]

    T.describe should equal("Foo[Boolean,String,Long,Int]")

    T.cast("hello") shouldBe empty
    T.cast(1L) shouldBe empty
    T.cast(foo) shouldBe empty
    T.cast(Foo(true, "hello", 1, 1)) shouldBe empty
    T.cast(Foo(true, "hello", 1L, 1)).value shouldBe Foo(true, "hello", 1L, 1)
  }

  it should "allow user-land Semigroup (Generic) derivation" in {
    import cats.Semigroup
    import cats.implicits._
    import cats.derived.semigroup._
    import cats.derived.semigroup.legacy._

    implicit val B: Semigroup[Boolean] = new Semigroup[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x & y
    }

    (foo |+| foo) should equal(Foo(true, "hellohello", "worldworld", 2))
  }

  it should "allow user-land JsonFormat (LabelledGeneric) derivation" in {
    import spray.json._
    import fommil.sjs.FamilyFormats._

    foo.toJson.compactPrint should equal("""{"a":true,"s":"hello","t":"world","i":1}""")
  }

}
