package fommil.stalagmite.spec

import testing.weakmemoised._

import _root_.scala._
import _root_.scala.Predef._
import java.io._

import com.google.common.testing.GcFinalization
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import org.scalatest._
import org.slf4j.LoggerFactory
import shapeless._

import scala.collection.mutable
import scala.util.Random

// intentionally parallel to try and flush out concurrency issues
class WeakMemoisedSpec extends FlatSpec with ParallelTestExecution {
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
    val out       = new ObjectOutputStream(bytes_out)

    out.writeObject(foo)
    out.close()

    val bytes_in = new ByteArrayInputStream(bytes_out.toByteArray)
    val in       = new ObjectInputStream(bytes_in)

    val recovered = in.readObject().asInstanceOf[Foo]

    recovered should equal(foo)
    recovered should be theSameInstanceAs (foo)
  }

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

  it should "allow user-land JsonFormat (LabelledGeneric) derivation" in {
    import spray.json._
    import fommil.sjs.FamilyFormats._

    implicit val J: JsonFormat[Foo] = cachedImplicit
  }

  it should "have memoised string fields" in {
    Foo(true, "stringy").s should be theSameInstanceAs Foo(false, "stringy").s
  }

  it should "ensure that memory remains" in {
    val memoryRemains =
      """
        |Fortune, fame
        |Mirror vain
        |Gone insane
        |But the memory remains
        |Heavy rings on fingers wave
        |Another star denies the grave
        |See the nowhere crowd,
        |cry the nowhere tears of honor
        |Like twisted vines that grow
        |That hide and swallow mansions whole
        |And dim the light of an already faded prima donna
        |Fortune, fame
        |Mirror vain
        |Gone insane...
        |Fortune, fame
        |Mirror vain
        |Gone insane...
        |But the memory remains
      """.stripMargin

    val data = memoryRemains.lines.zipWithIndex.map {
      case (line, i) => (i % 3 == 0, line)
    }.toList
    val foos = data.map {
      case (b, s) => Foo(b, s)
    }
    GcFinalization.awaitFullGc()

    Random.setSeed(4242L)
    for (_ <- 1 to 10000) {
      val ind    = Random.nextInt(foos.length)
      val (b, s) = data(ind)
      Foo(b, s) should be theSameInstanceAs foos(ind)
      Foo(b, s).s should be theSameInstanceAs foos(ind).s
    }
  }

  it should "withstand intense creating of random data №1" in {
    Random.setSeed(0xCAFEBABE)
    val data = (1 to 10000).map(
      _ => (Random.nextBoolean(), Random.nextString(Random.nextInt(100)))
    )
    GcFinalization.awaitFullGc()

    val foos = data.map {
      case (b, s) => Foo(b, s)
    }

    for (_ <- 1 to 100000) {
      val ind    = Random.nextInt(foos.length)
      val (b, s) = data(ind)
      Foo(b, s) should be theSameInstanceAs foos(ind)
      Foo(b, s).s should be theSameInstanceAs foos(ind).s
    }
  }

  it should "withstand intense creating of random data №2" in {
    Random.setSeed(0x0000BABE)
    val pool = mutable.ArrayBuffer.empty[Foo]
    for (i <- 1 to 500000) {
      val prob = Random.nextDouble()
      if (pool.isEmpty || prob < 0.05) {
        pool += Foo(Random.nextBoolean(), Random.nextString(1))
      } else if (prob < 0.5) {
        val ind = Random.nextInt(pool.length)
        pool += Foo(pool(ind).a, pool(ind).s)
      } else if (prob < 0.999) {
        val ind1 = Random.nextInt(pool.length)
        val ind2 = Random.nextInt(pool.length)
        if (pool(ind1).a == pool(ind2).a && pool(ind1).s == pool(ind2).s) {
          pool(ind1) should be theSameInstanceAs pool(ind2)
        } else {
          pool(ind1) shouldNot be theSameInstanceAs pool(ind2)
        }
      } else {
        GcFinalization.awaitFullGc()
      }
    }
  }
}
