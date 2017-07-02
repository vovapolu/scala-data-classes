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
    metaFoo should equal(metaFoo)
    metaFoo should not be theSameInstanceAs(FooMetaCaseClass(true, "hello", "world", 1))
    metaFoo should not equal FooMetaCaseClass(false, "hello", "world", 1)
    metaFoo.toString should equal("FooMetaCaseClass(true,hello,world,1)")
    FooMetaCaseClass.toString should equal("FooMetaCaseClass")
  }

  it should "not expose its constructor" in {
    """new FooMetaCaseClass(true, "hello", "world", 1)""" shouldNot compile

    """new FooMetaCaseClass(true, "hello", "world")""" shouldNot compile

    """new FooMetaCaseClass()""" shouldNot compile
  }

  it should "expose its fields" in {
    metaFoo.a should equal(true)
    metaFoo.s should equal("hello")
    metaFoo.t should equal("world")
    metaFoo.i should equal(1)
  }

  it should "have default parameters" in {
    FooMetaCaseClass(false, "goodbye", 13) should equal(FooMetaCaseClass(false, "goodbye", 13, 0))
  }

  it should "have a copy method" in {
    metaFoo.copy(a = false) should equal(FooMetaCaseClass(false, "hello", "world", 1))
    metaFoo.copy(s = "foo") should equal(FooMetaCaseClass(true, "foo", "world", 1))
    metaFoo.copy(t = 13) should equal(FooMetaCaseClass(true, "hello", 13, 1))
    metaFoo.copy(i = -1) should equal(FooMetaCaseClass(true, "hello", "world", -1))
  }

  it should "have a pattern matcher" in {
    metaFoo should matchPattern { case FooMetaCaseClass(true, "hello", "world", 1) => }
  }

  it should "implement Product" in {
    metaFoo.productPrefix should equal("FooMetaCaseClass")
    metaFoo.productIterator.toList should contain theSameElementsInOrderAs (List(true, "hello", "world", 1))
  }

  it should "be serialisable" in {
    // should really check with the serialised form of the equivalent case class

    val bytes_out = new ByteArrayOutputStream
    val out = new ObjectOutputStream(bytes_out)

    out.writeObject(metaFoo)
    out.close()

    val bytes_in = new ByteArrayInputStream(bytes_out.toByteArray())
    val in = new ObjectInputStream(bytes_in)

    val recovered = in.readObject().asInstanceOf[FooMetaCaseClass[String]]

    recovered should equal(metaFoo)
    recovered should not be theSameInstanceAs(metaFoo)
  }

  it should "have a Generic" in {
    implicit val G: Generic[FooMetaCaseClass[String]] = cachedImplicit
    import G._

    from(to(metaFoo)) should equal(metaFoo)
  }

  it should "have a LabelledGeneric" in {
    implicit val LG: LabelledGeneric[FooMetaCaseClass[String]] = cachedImplicit
    import LG._

    from(to(metaFoo)) should equal(metaFoo)
  }

  it should "have a Typeable" in {
    val T = Typeable[FooMetaCaseClass[Long]]

    T.describe should equal("FooMetaCaseClass[Boolean,String,Long,Int]")

    T.cast("hello") shouldBe empty
    T.cast(1L) shouldBe empty
    T.cast(metaFoo) shouldBe empty
    T.cast(FooMetaCaseClass(true, "hello", 1, 1)) shouldBe empty
    T.cast(FooMetaCaseClass(true, "hello", 1L, 1)).value shouldBe FooMetaCaseClass(true, "hello", 1L, 1)
  }

  it should "allow user-land Semigroup (Generic) derivation" in {
    import cats.Semigroup
    import cats.implicits._
    import cats.derived.semigroup._
    import cats.derived.semigroup.legacy._

    implicit val B: Semigroup[Boolean] = new Semigroup[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x & y
    }

    (metaFoo |+| metaFoo) should equal(FooMetaCaseClass(true, "hellohello", "worldworld", 2))
  }

  it should "allow user-land JsonFormat (LabelledGeneric) derivation" in {
    import spray.json._
    import fommil.sjs.FamilyFormats._

    metaFoo.toJson.compactPrint should equal("""{"a":true,"s":"hello","t":"world","i":1}""")
  }

}
