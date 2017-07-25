package stalagmite

import testing.memoised._
import testing.meta._

import _root_.scala._
import _root_.scala.Predef._
import java.io._

import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import shapeless._

// intentionally parallel to try and flush out concurrency issues
class MemoisedSpec extends FlatSpec with ParallelTestExecution {

  val foo     = Foo(true, "hello")
  val fooMeta = FooMetaMemoised(true, "hello")

  "@data(memoised) class Foo" should
    "have equals, hashCode and toString defined" in {
    fooMeta.hashCode shouldBe 1289111417
    fooMeta should equal(fooMeta)
    fooMeta should be theSameInstanceAs FooMetaMemoised(true, "hello")
    fooMeta should not equal FooMetaMemoised(false, "hello")
    fooMeta.toString should be theSameInstanceAs fooMeta.toString // memoiseToString
    FooMetaMemoised.toString should equal("FooMetaMemoised")
  }

  it should "not expose its constructor" in {
    """new FooMetaMemoised(true, "hello")""" shouldNot compile

    """new FooMetaMemoised()""" shouldNot compile
  }

  it should "expose its fields" in {
    fooMeta.a should equal(true)
    fooMeta.s should equal("hello")
  }

  it should "have a copy method" in {
    fooMeta.copy(a = false) should be theSameInstanceAs
      FooMetaMemoised(false, "hello")
    fooMeta.copy(s = "foo") should be theSameInstanceAs
      FooMetaMemoised(true, "foo")
  }

  it should "have a pattern matcher" in {
    fooMeta should matchPattern { case FooMetaMemoised(true, "hello") => }
  }

  it should "be serialisable (with memoisation holding)" in {
    val bytes_out = new ByteArrayOutputStream
    val out       = new ObjectOutputStream(bytes_out)

    out.writeObject(fooMeta)
    out.close()

    val bytes_in = new ByteArrayInputStream(bytes_out.toByteArray())
    val in       = new ObjectInputStream(bytes_in)

    val recovered = in.readObject().asInstanceOf[FooMetaMemoised]

    recovered should equal(fooMeta)
    recovered should be theSameInstanceAs fooMeta
  }

  it should "have a Generic" in {
    implicit val G: Generic[FooMetaMemoised] = cachedImplicit
    import G._

    from(to(fooMeta)) should equal(fooMeta)
    from(to(fooMeta)) should be theSameInstanceAs fooMeta
  }

  it should "have a LabelledGeneric" in {
    implicit val LG: LabelledGeneric[FooMetaMemoised] = cachedImplicit
    import LG._

    from(to(fooMeta)) should equal(fooMeta)
    from(to(fooMeta)) should be theSameInstanceAs fooMeta
  }

  it should "have a Typeable" in {
    val T = Typeable[FooMetaMemoised]

    T.describe should equal("FooMetaMemoised[Boolean,String]")

    T.cast("hello") shouldBe empty
    T.cast(1L) shouldBe empty
    T.cast(fooMeta).value shouldBe fooMeta
    T.cast(fooMeta).value should be theSameInstanceAs fooMeta
  }

  it should "allow user-land Semigroup (Generic) derivation" in {
    import cats.Semigroup
    import cats.implicits._
    import cats.derived.semigroup._
    import cats.derived.semigroup.legacy._

    implicit val B: Semigroup[Boolean] = new Semigroup[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x & y
    }

    (fooMeta |+| fooMeta) should be theSameInstanceAs
      FooMetaMemoised(true, "hellohello")

  }

  // redundant, just using it becuase I am familiar with the required imports
  it should "allow user-land JsonFormat (LabelledGeneric) derivation" in {
    import spray.json._
    import fommil.sjs.FamilyFormats._

    fooMeta.toJson.compactPrint should equal("""{"a":true,"s":"hello"}""")
  }

  it should "have memoised string fields" in {
    // constructing the String on the heap, so the compiler doesn't intern
    // we can't guarantee this if memoiseStrong=false (unless using string interning)
    FooMetaMemoised(true, 1337133742.toString).s should be theSameInstanceAs
      FooMetaMemoised(false, 1337133742.toString).s

  }

  it should "apply weak memoisation" in {
    val fooMetaWeak = FooMetaMemoisedWeak(true, "hello")
    fooMetaWeak should be theSameInstanceAs FooMetaMemoisedWeak(true, "hello")
  }
}
