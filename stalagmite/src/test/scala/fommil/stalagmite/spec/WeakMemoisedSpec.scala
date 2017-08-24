package fommil.stalagmite.spec

import testing.weakmemoised._
import testing.meta._

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

  val foo     = Foo(true, "hello")
  val fooMeta = FooMetaMemoisedWeak(true, "hello")

  "@data(memoised) class Foo" should
    "have equals, hashCode and toString defined" in {
    fooMeta.hashCode shouldBe 1289111417
    fooMeta should equal(fooMeta)
    fooMeta should be theSameInstanceAs (FooMetaMemoisedWeak(true, "hello"))
    fooMeta should not equal (FooMetaMemoisedWeak(false, "hello"))
    fooMeta.toString should equal("FooMetaMemoisedWeak(true,hello)")
    FooMetaMemoisedWeak.toString should equal("FooMetaMemoisedWeak")
  }

  it should "not expose its constructor" in {
    """new FooMetaMemoisedWeak(true, "hello")""" shouldNot compile

    """new FooMetaMemoisedWeak()""" shouldNot compile
  }

  it should "expose its fields" in {
    fooMeta.a should equal(true)
    fooMeta.s should equal("hello")
  }

  it should "have a copy method" in {
    fooMeta.copy(a = false) should equal(FooMetaMemoisedWeak(false, "hello"))
    fooMeta.copy(s = "fooMeta") should
      equal(FooMetaMemoisedWeak(true, "fooMeta"))
  }

  it should "have a pattern matcher" in {
    fooMeta should matchPattern { case FooMetaMemoisedWeak(true, "hello") => }
  }

  it should "be serialisable (with memoisation holding)" in {
    val bytes_out = new ByteArrayOutputStream
    val out       = new ObjectOutputStream(bytes_out)

    out.writeObject(fooMeta)
    out.close()

    val bytes_in = new ByteArrayInputStream(bytes_out.toByteArray)
    val in       = new ObjectInputStream(bytes_in)

    val recovered = in.readObject().asInstanceOf[FooMetaMemoisedWeak]

    recovered should equal(fooMeta)
    recovered should be theSameInstanceAs (fooMeta)
  }

  it should "have a Generic" in {
    implicit val G: Generic[FooMetaMemoisedWeak] = cachedImplicit
    import G._

    from(to(fooMeta)) should equal(fooMeta)
    from(to(fooMeta)) should be theSameInstanceAs (fooMeta)
  }

  it should "have a LabelledGeneric" in {
    implicit val LG: LabelledGeneric[FooMetaMemoisedWeak] = cachedImplicit
    import LG._

    from(to(fooMeta)) should equal(fooMeta)
    from(to(fooMeta)) should be theSameInstanceAs (fooMeta)
  }

  it should "have a Typeable" in {
    val T = Typeable[FooMetaMemoisedWeak]

    T.describe should equal("FooMetaMemoisedWeak[Boolean,String]")

    T.cast("hello") shouldBe empty
    T.cast(1L) shouldBe empty
    T.cast(fooMeta).value shouldBe fooMeta
    T.cast(fooMeta).value should be theSameInstanceAs (fooMeta)
  }

  it should "allow user-land Show (Generic) derivation" in {
    import cats.Semigroup
    import cats.implicits._
    import cats.derived.semigroup._
    import cats.derived.semigroup.legacy._

    implicit val B: Semigroup[Boolean] = new Semigroup[Boolean] {
      override def combine(x: Boolean, y: Boolean): Boolean = x & y
    }

    implicit val S: Semigroup[FooMetaMemoisedWeak] = cachedImplicit

    S.combine(fooMeta, fooMeta) should equal(
      FooMetaMemoisedWeak(true, "hellohello")
    )
  }

  it should "allow user-land JsonFormat (LabelledGeneric) derivation" in {
    import spray.json._
    import fommil.sjs.FamilyFormats._

    implicit val J: JsonFormat[FooMetaMemoisedWeak] = cachedImplicit
  }

  it should "have memoised string fields" in {
    FooMetaMemoisedWeak(true, "stringy").s should
      be theSameInstanceAs FooMetaMemoisedWeak(false, "stringy").s
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
      case (b, s) => FooMetaMemoisedWeak(b, s)
    }
    GcFinalization.awaitFullGc()

    Random.setSeed(4242L)
    for (_ <- 1 to 10000) {
      val ind    = Random.nextInt(foos.length)
      val (b, s) = data(ind)
      FooMetaMemoisedWeak(b, s) should be theSameInstanceAs foos(ind)
      FooMetaMemoisedWeak(b, s).s should be theSameInstanceAs foos(ind).s
    }
  }

  it should "withstand intense creating of random data №1" in {
    Random.setSeed(0xCAFEBABE)
    val data = (1 to 10000).map(
      _ => (Random.nextBoolean(), Random.nextString(Random.nextInt(100)))
    )
    GcFinalization.awaitFullGc()

    val foos = data.map {
      case (b, s) => FooMetaMemoisedWeak(b, s)
    }

    for (_ <- 1 to 100000) {
      val ind    = Random.nextInt(foos.length)
      val (b, s) = data(ind)
      FooMetaMemoisedWeak(b, s) should be theSameInstanceAs foos(ind)
      FooMetaMemoisedWeak(b, s).s should be theSameInstanceAs foos(ind).s
    }
  }

  it should "withstand intense creating of random data №2" in {
    Random.setSeed(0x0000BABE)
    val pool = mutable.ArrayBuffer.empty[FooMetaMemoisedWeak]
    for (i <- 1 to 500000) {
      val prob = Random.nextDouble()
      if (pool.isEmpty || prob < 0.05) {
        pool += FooMetaMemoisedWeak(Random.nextBoolean(), Random.nextString(1))
      } else if (prob < 0.5) {
        val ind = Random.nextInt(pool.length)
        pool += FooMetaMemoisedWeak(pool(ind).a, pool(ind).s)
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
