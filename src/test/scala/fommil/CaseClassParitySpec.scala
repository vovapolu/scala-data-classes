package fommil

import _root_.scala._
import _root_.scala.Predef._

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
  val fooMeta = FooMeta(true, "hello", "world", 1)

  "@data(product) class Foo[+]" should "have equals, hashCode and toString defined" in {
    fooMeta.hashCode shouldBe -1034845328
    fooMeta should equal(fooMeta)
    fooMeta should not be theSameInstanceAs(FooMeta(true, "hello", "world", 1))
    fooMeta should not equal FooMeta(false, "hello", "world", 1)
    fooMeta.toString should equal("FooMeta(true,hello,world,1)")
    FooMeta.toString should equal("FooMeta")
  }

  it should "not expose its constructor" in {
    """new FooMeta(true, "hello", "world", 1)""" shouldNot compile

    """new FooMeta(true, "hello", "world")""" shouldNot compile

    """new FooMeta()""" shouldNot compile
  }

  it should "expose its fields" in {
    fooMeta.a should equal(true)
    fooMeta.s should equal("hello")
    fooMeta.t should equal("world")
    fooMeta.i should equal(1)
  }

  it should "have default parameters" in {
    FooMeta(false, "goodbye", 13) should equal(FooMeta(false, "goodbye", 13, 0))
  }

  it should "have a copy method" in {
    fooMeta.copy(a = false) should equal(FooMeta(false, "hello", "world", 1))
    fooMeta.copy(s = "foo") should equal(FooMeta(true, "foo", "world", 1))
    fooMeta.copy(t = 13) should equal(FooMeta(true, "hello", 13, 1))
    fooMeta.copy(i = -1) should equal(FooMeta(true, "hello", "world", -1))
  }

  it should "have a pattern matcher" in {
    fooMeta should matchPattern { case FooMeta(true, "hello", "world", 1) => }
  }

  it should "implement Product" in {
    fooMeta.productPrefix should equal("FooMeta")
    fooMeta.productIterator.toList should contain theSameElementsInOrderAs (List(true, "hello", "world", 1))
  }

  it should "be serialisable" in {
    // should really check with the serialised form of the equivalent case class

    val bytes_out = new ByteArrayOutputStream
    val out = new ObjectOutputStream(bytes_out)

    out.writeObject(fooMeta)
    out.close()

    val bytes_in = new ByteArrayInputStream(bytes_out.toByteArray())
    val in = new ObjectInputStream(bytes_in)

    val recovered = in.readObject().asInstanceOf[FooMeta[String]]

    recovered should equal(fooMeta)
    recovered should not be theSameInstanceAs(fooMeta)
  }

  it should "have a Generic" in {
    implicit val G: Generic[FooMeta[String]] = cachedImplicit
    import G._

    from(to(fooMeta)) should equal(fooMeta)
  }

  it should "have a LabelledGeneric" in {
    implicit val LG: LabelledGeneric[FooMeta[String]] = cachedImplicit
    import LG._

    from(to(fooMeta)) should equal(fooMeta)
  }

  it should "have a Typeable" in {
    val T = Typeable[FooMeta[Long]]

    T.describe should equal("FooMeta[Boolean,String,Long,Int]")

    T.cast("hello") shouldBe empty
    T.cast(1L) shouldBe empty
    T.cast(fooMeta) shouldBe empty
    T.cast(FooMeta(true, "hello", 1, 1)) shouldBe empty
    T.cast(FooMeta(true, "hello", 1L, 1)).value shouldBe FooMeta(true, "hello", 1L, 1)
  }

  it should "allow user-land Semigroup (Generic) derivation" in {
    import cats.Semigroup
    import cats.implicits._
    import cats.derived.semigroup._
    import cats.derived.semigroup.legacy._

    implicit val B: Semigroup[Boolean] = new Semigroup[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x & y
    }

    (fooMeta |+| fooMeta) should equal(FooMeta(true, "hellohello", "worldworld", 2))
  }

  it should "allow user-land JsonFormat (LabelledGeneric) derivation" in {
    import spray.json._
    import fommil.sjs.FamilyFormats._

    fooMeta.toJson.compactPrint should equal("""{"a":true,"s":"hello","t":"world","i":1}""")
  }

}
