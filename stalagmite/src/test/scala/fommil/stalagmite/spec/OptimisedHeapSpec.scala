package fommil.stalagmite.spec

import testing.optimiseheap._
import testing.meta._

import _root_.scala._
import _root_.scala.Predef._
import java.io._

import org.scalactic.anyvals.PosInt
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import shapeless._

class OptimisedHeapSpec
    extends FlatSpec
    with ParallelTestExecution
    with GeneratorDrivenPropertyChecks {
  val foo = Foo(Option(true), Option(false), Option("world"))
  val fooMeta =
    FooMetaOptimiseHeap(Option(true), Option(false), Option("world"))

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = PosInt(100))

  "@data(product) class FooMetaOptimiseHeap[+]" should
    "have equals, hashCode and toString defined" in {
    fooMeta.hashCode shouldBe 626589729
    fooMeta should equal(fooMeta)
    fooMeta should not be theSameInstanceAs(
      FooMetaOptimiseHeap(Option(true), Option(false), Option("world"))
    )
    fooMeta should not equal FooMetaOptimiseHeap(Option(false),
                                                 Option(false),
                                                 Option("world"))

    fooMeta.toString should equal(
      "FooMetaOptimiseHeap(Some(true),Some(false),Some(world))"
    )
    FooMetaOptimiseHeap.toString should equal("FooMetaOptimiseHeap")
  }

  it should "not expose its constructor" in {
    """new FooMetaOptimiseHeap(
      Option(true), Option(false), Option("world"))""" shouldNot compile

    """new FooMetaOptimiseHeap()""" shouldNot compile
  }

  it should "expose its fields" in {
    fooMeta.a.value should equal(true)
    fooMeta.b.value should equal(false)
    fooMeta.s.value should equal("world")
  }

  it should "correctly return a wide range parameters as fields" in {
    forAll { (a: Option[Boolean], b: Option[Boolean], s: Option[String]) =>
      val f = FooMetaOptimiseHeap(a, b, s)
      f.a should equal(a)
      f.b should equal(b)
      f.s should equal(s)
    }
  }

  it should "have a copy method" in {
    fooMeta.copy(a = Option(false)) should equal(
      FooMetaOptimiseHeap(Option(false), Option(false), Option("world"))
    )
    fooMeta.copy(b = Option(true)) should equal(
      FooMetaOptimiseHeap(Option(true), Option(true), Option("world"))
    )
    fooMeta.copy(s = Option("foo")) should equal(
      FooMetaOptimiseHeap(Option(true), Option(false), Option("foo"))
    )
  }

  it should "have a pattern matcher" in {
    fooMeta should matchPattern {
      case FooMetaOptimiseHeap(Some(true), Some(false), Some("world")) =>
    }
  }

  it should "be serialisable" in {
    // should really check that this is using the public form, not the internal form

    val bytes_out = new ByteArrayOutputStream
    val out       = new ObjectOutputStream(bytes_out)

    out.writeObject(fooMeta)
    out.close()

    val bytes_in = new ByteArrayInputStream(bytes_out.toByteArray())
    val in       = new ObjectInputStream(bytes_in)

    val recovered = in.readObject().asInstanceOf[FooMetaOptimiseHeap]

    recovered should equal(fooMeta)
    recovered should not be theSameInstanceAs(fooMeta)
  }

  it should "have a Generic" in {
    implicit val G: Generic[FooMetaOptimiseHeap] = cachedImplicit
    import G._

    from(to(fooMeta)) should equal(fooMeta)
  }

  it should "have a LabelledGeneric" in {
    implicit val LG: LabelledGeneric[FooMetaOptimiseHeap] = cachedImplicit
    import LG._

    from(to(fooMeta)) should equal(fooMeta)
  }

  it should "have a Typeable" in {
    val T = Typeable[FooMetaOptimiseHeap]

    T.describe should equal(
      "FooMetaOptimiseHeap[Option[Boolean],Option[Boolean],Option[String]]"
    )

    T.cast("hello") shouldBe empty
    T.cast(1L) shouldBe empty
    T.cast(fooMeta).value should equal(fooMeta)
  }

  it should "allow user-land Semigroup (Generic) derivation" in {
    import cats.Semigroup
    import cats.implicits._
    import cats.derived.semigroup._
    import cats.derived.semigroup.legacy._

    implicit val B: Semigroup[Boolean] = new Semigroup[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x & y
    }

    (fooMeta |+| fooMeta) should equal(
      FooMetaOptimiseHeap(Option(true), Option(false), Option("worldworld"))
    )
  }

  it should "allow user-land JsonFormat (LabelledGeneric) derivation" in {
    import spray.json._
    import fommil.sjs.FamilyFormats._

    fooMeta.toJson.compactPrint should equal(
      """{"a":true,"b":false,"s":"world"}"""
    )
  }

  it should "not allow null or Some(null) parameters for optimised fields" in {

    intercept[NullPointerException] {
      FooMetaOptimiseHeap(null, Option(true), Option("hello"))
    }

    intercept[NullPointerException] {
      FooMetaOptimiseHeap(Option(true), null, Option("hello"))
    }

    intercept[NullPointerException] {
      FooMetaOptimiseHeap(Option(true), Option(true), null)
    }

    intercept[NullPointerException] {
      FooMetaOptimiseHeap(Option(true), Option(true), Some(null))
    }
  }
}
