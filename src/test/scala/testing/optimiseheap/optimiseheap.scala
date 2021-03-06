package testing.optimiseheap

import _root_.scala._
import _root_.scala.Predef._

// @data(
//   optimiseHeapOptions = true,
//   optimiseHeapBooleans = true,
//   optimiseHeapStrings = true,
// )
// class Foo(a: Option[Boolean], b: Option[Boolean], s: Option[String])
final class Foo private (
  private[this] var _bitmask: Long,
  private[this] var _s: Array[Byte]
) extends scala.Serializable {

  def a: Option[Boolean] =
    if ((_bitmask & (1 << 0)) != 0) None
    else Some((_bitmask & (1 << 1)) != 0) // optimiseHeapBooleans
  def b: Option[Boolean] =
    if ((_bitmask & (1 << 2)) != 0) None
    else Some((_bitmask & (1 << 3)) != 0) // optimiseHeapBooleans
  def s: Option[String] =
    if (_s == null) None
    else Some(new String(_s)) // optimiseHeapStrings

  def copy(
    a: Option[Boolean] = a,
    b: Option[Boolean] = b,
    s: Option[String] = s
  ): Foo = Foo(a, b, s)

  override def toString(): String = s"Foo($a,$b,$s)"
  // I'm not going to lie, hashCode/equals is slower... this is about trading CPU for heap
  // (note that memoiseHashCode could be done before construction to avoid unpacking)
  override def hashCode(): Int =
    a.hashCode + 13 * (b.hashCode + 13 * s.hashCode)
  override def equals(o: Any): Boolean = o match {
    case that: Foo => (this eq that) || (a == that.a && s == that.s)
    case _         => false
  }

  @throws[java.io.IOException]
  private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = {
    // note we are checking compile time Serializable
    out.writeObject(a: Serializable)
    out.writeObject(b: Serializable)
    out.writeObject(s: Serializable)
  }
  @throws[java.io.IOException]
  @throws[java.lang.ClassNotFoundException]
  private[this] def readObject(in: java.io.ObjectInputStream): Unit = {
    val a      = in.readObject().asInstanceOf[Option[Boolean]]
    val b      = in.readObject().asInstanceOf[Option[Boolean]]
    val s      = in.readObject().asInstanceOf[Option[String]]
    val packed = Foo.pack(a, b, s)
    _bitmask = packed._1
    _s = packed._2
  }
  @throws[java.io.ObjectStreamException]
  private[this] def readResolve(): Any = Foo(a, b, s)

}

final object Foo
    extends ((Option[Boolean], Option[Boolean], Option[String]) => Foo)
    with scala.Serializable {
  override def toString = "Foo"

  @throws[java.io.IOException]
  private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = ()
  @throws[java.io.IOException]
  @throws[java.lang.ClassNotFoundException]
  private[this] def readObject(in: java.io.ObjectInputStream): Unit = ()
  @throws[java.io.ObjectStreamException]
  private[this] def readResolve(raw: Foo.type): Any = Foo

  // unpack is available on the instance, i.e. field access
  private def pack(a: Option[Boolean],
                   b: Option[Boolean],
                   s: Option[String]): (Long, Array[Byte]) = {
    var _bitmask: Long = 0L

    if (a == None) _bitmask |= (1 << 0)
    else if (a.get) _bitmask |= (1 << 1)

    if (b == None) _bitmask |= (1 << 2)
    else if (b.get) _bitmask |= (1 << 3)

    val _s = if (s == None) null else s.get.getBytes

    (_bitmask, _s)
  }

  def apply(a: Option[Boolean], b: Option[Boolean], s: Option[String]): Foo = {
    val packed = pack(a, b, s)
    val foo    = new Foo(packed._1, packed._2)
    foo.synchronized(foo) // safe publish
  }
  def unapply(
    f: Foo
  ): Option[(Option[Boolean], Option[Boolean], Option[String])] =
    Some((f.a, f.b, f.s))

  import shapeless.labelled.{ field, FieldType }
  import shapeless.syntax.singleton._
  import shapeless.{ ::, Generic, HNil, LabelledGeneric, Typeable }
  val a_tpe = 'a.narrow
  val b_tpe = 'b.narrow
  val s_tpe = 's.narrow
  implicit val TypeableFoo: Typeable[Foo] = new Typeable[Foo] {
    override def cast(t: Any): Option[Foo] = t match {
      case f: Foo => Some(f) // no type params, so trivial
      case _      => None
    }
    override def describe: String =
      s"Foo[Option[Boolean],Option[Boolean],Option[String]]"
  }

  implicit val LabelledGenericFoo
    : LabelledGeneric.Aux[Foo,
                          FieldType[a_tpe.type, Option[Boolean]] ::
                            FieldType[b_tpe.type, Option[Boolean]] ::
                            FieldType[s_tpe.type, Option[String]] ::
                            HNil] =
    new LabelledGeneric[Foo] {
      override type Repr =
        FieldType[a_tpe.type, Option[Boolean]] ::
          FieldType[b_tpe.type, Option[Boolean]] ::
          FieldType[s_tpe.type, Option[String]] ::
          HNil
      override def to(f: Foo): Repr =
        field[a_tpe.type](f.a) ::
          field[b_tpe.type](f.b) ::
          field[s_tpe.type](f.s) ::
          HNil
      override def from(r: Repr): Foo = GenericFoo.from(r)
    }

  implicit val GenericFoo
    : Generic.Aux[Foo, Option[Boolean] :: Option[Boolean] :: Option[String] :: HNil] =
    new Generic[Foo] {
      override type Repr =
        Option[Boolean] :: Option[Boolean] :: Option[String] :: HNil
      override def to(f: Foo): Repr = LabelledGenericFoo.to(f)
      override def from(r: Repr): Foo = r match {
        case a :: b :: s :: HNil => Foo(a, b, s)
      }
    }

}
