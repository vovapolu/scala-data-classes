package testing.memoised

import _root_.scala._
import _root_.scala.Predef._

// @data(
//   memoise = true,
//   memoiseRefs = Seq('s),
//   memoiseHashCode = true,
//   memoiseToString = true,
//   memoiseStrong = true,
//   memoiseStringsIntern = false,
//   companionExtends = true
// )
// class Foo(a: Boolean, s: String)
final class Foo private (
  private[this] var _a: Boolean,
  private[this] var _s: String
) extends scala.Serializable {

  def a: Boolean = _a
  def s: String = _s

  def copy(a: Boolean = a, s: String = s): Foo = Foo(a, s)

  // allows the user to re-memoise if the Interner was flushed. Only
  // generated if memoise = true & memoiseStrong = false. Called
  // .intern because there is precedent with String.intern
  // def intern: Foo = Foo(a, s) FIXME memoise && !memoiseStrong is false, is it right?

  override val toString: String = s"Foo($a,$s)"
  override val hashCode: Int = a.hashCode + 13 * s.hashCode

  // // not added if memoiseStrong=true. hashCode shortcut only added if memoiseHashCode=true
  // override def equals(o: Any): Boolean = o match {
  //   case that: Foo => (this eq that) || (hashCode == that.hashCode && a == that.a && s == that.s)
  //   case _         => false
  // }

  @throws[java.io.IOException]
  private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.writeBoolean(a)
    out.writeUTF(s)
  }
  @throws[java.io.IOException]
  @throws[java.lang.ClassNotFoundException]
  private[this] def readObject(in: java.io.ObjectInputStream): Unit = {
    _a = in.readBoolean()
    _s = in.readUTF()
  }
  @throws[java.io.ObjectStreamException]
  private[this] def readResolve(): Any = Foo(a, s)

}

// companionExtends = true
final object Foo extends ((Boolean, String) => Foo) with scala.Serializable {
  override def toString = "Foo"

  // incase somebody serialises the companion (it happens!)
  @throws[java.io.IOException]
  private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = ()
  @throws[java.io.IOException]
  @throws[java.lang.ClassNotFoundException]
  private[this] def readObject(in: java.io.ObjectInputStream): Unit = ()
  @throws[java.io.ObjectStreamException]
  private[this] def readResolve(raw: Foo.type): Any = Foo

  // this wrapper is only needed when memoiseStrong=true (to force value equality)
  private class FooWithValueEquality(val f: Foo) {
    override def toString: String = f.toString
    override def hashCode: Int = f.hashCode
    override def equals(o: Any): Boolean = o match {
      // only use the hashCode shortcut if memoiseHashCode=true
      case that: FooWithValueEquality if hashCode == that.hashCode => f.a == that.f.a && f.s == that.f.s
      case _ => false
    }
  }

  // memoiseStrong = true, so use a StrongInterner. Shame there is no SoftInterner
  private[this] val memoised_cache = com.google.common.collect.Interners.newStrongInterner[FooWithValueEquality]()
  private[this] val memoisedRef_cache = com.google.common.collect.Interners.newStrongInterner[AnyRef]()
  def apply(a: Boolean, s: String): Foo = {
    // special case available for String.intern with memoizeStringsIntern=true
    val s_memoised = memoisedRef_cache.intern(s).asInstanceOf[String]
    val created = new Foo(a, s_memoised)
    val safe = created.synchronized(created) // safe publish vars
    memoised_cache.intern(new FooWithValueEquality(safe)).f
  }
  def unapply(f: Foo): Option[(Boolean, String)] = Some((f.a, f.s))

  import shapeless.{::, HNil, Generic, LabelledGeneric, Typeable}
  import shapeless.labelled.{FieldType, field}
  import shapeless.syntax.singleton._
  val a_tpe = 'a.narrow
  val s_tpe = 's.narrow
  implicit val TypeableFoo: Typeable[Foo] = new Typeable[Foo] {
    override def cast(t: Any): Option[Foo] = t match {
      case f: Foo => Some(f) // no type params, so trivial
      case _      => None
    }
    override def describe: String = s"Foo[Boolean,String]"
  }

  implicit val LabelledGenericFoo: LabelledGeneric.Aux[Foo, FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: HNil] =
    new LabelledGeneric[Foo] {
      override type Repr = FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: HNil
      override def to(f: Foo): Repr = field[a_tpe.type](f.a) :: field[s_tpe.type](f.s) :: HNil
      override def from(r: Repr): Foo = GenericFoo.from(r)
    }

  implicit val GenericFoo: Generic.Aux[Foo, Boolean :: String :: HNil] =
    new Generic[Foo] {
      override type Repr = Boolean :: String :: HNil
      override def to(f: Foo): Repr = LabelledGenericFoo.to(f)
      override def from(r: Repr): Foo = r match {
        case a :: s :: HNil => Foo(a, s)
      }
    }

}
