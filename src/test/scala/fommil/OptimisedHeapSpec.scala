package fommil

import java.io._

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalactic.anyvals.PosInt
import testing.optimiseheap._
import shapeless._

class OptimisedHeapSpec extends FlatSpec with ParallelTestExecution with GeneratorDrivenPropertyChecks {
  val foo = Foo(Option(true), Option(false), Option("world"))

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(100))

  "@data(product) class Foo[+]" should "have equals, hashCode and toString defined" in {
    foo.hashCode shouldBe 626589729
    foo should equal(foo)
    foo should not be theSameInstanceAs(Foo(Option(true), Option(false), Option("world")))
    foo should not equal (Foo(Option(false), Option(false), Option("world")))
    foo.toString should equal("Foo(Some(true),Some(false),Some(world))")
    Foo.toString should equal("Foo")
  }

  it should "not expose its constructor" in {
    """new Foo(Option(true), Option(false), Option("world"))""" shouldNot compile

    """new Foo()""" shouldNot compile
  }

  it should "expose its fields" in {
    foo.a.value should equal(true)
    foo.b.value should equal(false)
    foo.s.value should equal("world")
  }

  it should "correctly return a wide range parameters as fields" in {
    forAll { (a: Option[Boolean], b: Option[Boolean], s: Option[String]) =>
      val f = Foo(a, b, s)
      f.a should equal(a)
      f.b should equal(b)
      f.s should equal(s)
    }
  }

  it should "have a copy method" in {
    foo.copy(a = Option(false)) should equal(Foo(Option(false), Option(false), Option("world")))
    foo.copy(b = Option(true)) should equal(Foo(Option(true), Option(true), Option("world")))
    foo.copy(s = Option("foo")) should equal(Foo(Option(true), Option(false), Option("foo")))
  }

  it should "have a pattern matcher" in {
    foo should matchPattern { case Foo(Some(true), Some(false), Some("world")) => }
  }

  it should "be serialisable" in {
    // should really check that this is using the public form, not the internal form

    val bytes_out = new ByteArrayOutputStream
    val out = new ObjectOutputStream(bytes_out)

    out.writeObject(foo)
    out.close()

    val bytes_in = new ByteArrayInputStream(bytes_out.toByteArray())
    val in = new ObjectInputStream(bytes_in)

    val recovered = in.readObject().asInstanceOf[Foo]

    recovered should equal(foo)
    recovered should not be theSameInstanceAs(foo)
  }

  it should "have a Generic" in {
    implicit val G: Generic[Foo] = cachedImplicit
    import G._

    from(to(foo)) should equal(foo)
  }

  it should "have a LabelledGeneric" in {
    implicit val LG: LabelledGeneric[Foo] = cachedImplicit
    import LG._

    from(to(foo)) should equal(foo)
  }

  it should "have a Typeable" in {
    val T = Typeable[Foo]

    T.describe should equal("Foo[Option[Boolean],Option[Boolean],Option[String]]")

    T.cast("hello") shouldBe empty
    T.cast(1L) shouldBe empty
    T.cast(foo).value should equal(foo)
    T.cast(foo).value should be theSameInstanceAs (foo)
  }

  it should "allow user-land Semigroup (Generic) derivation" in {
    import cats.Semigroup
    import cats.implicits._
    import cats.derived.semigroup._
    import cats.derived.semigroup.legacy._

    implicit val B: Semigroup[Boolean] = new Semigroup[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x & y
    }

    (foo |+| foo) should equal(Foo(Option(true), Option(true), Option("worldworld")))
  }

  it should "allow user-land JsonFormat (LabelledGeneric) derivation" in {
    import spray.json._
    import fommil.sjs.FamilyFormats._

    foo.toJson.compactPrint should equal("""{"a":true,"b":false,"s":"world"}""")
  }

  it should "not allow null or Some(null) parameters for optimised fields" in {

    intercept[NullPointerException] {
      Foo(null, Option(true), Option("hello"))
    }

    intercept[NullPointerException] {
      Foo(Option(true), null, Option("hello"))
    }

    intercept[NullPointerException] {
      Foo(Option(true), Option(true), null)
    }

    intercept[NullPointerException] {
      Foo(Option(true), Option(true), Some(null))
    }

  }
}
