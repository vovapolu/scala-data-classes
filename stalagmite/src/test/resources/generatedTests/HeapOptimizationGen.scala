//optimiseHeapOptions optimiseHeapBooleans optimiseHeapStrings serializable shapeless
class Foo(a: Option[Boolean], b: Option[Boolean], s: Option[String], i: Option[Int])
//---
{
  final class Foo private (private[this] var _s: _root_.scala.Array[_root_.scala.Byte], private[this] var _i: Int, private[this] var _bitmask: _root_.scala.Long) extends _root_.scala.Serializable {
    override def equals(thatAny: _root_.scala.Any): _root_.scala.Boolean = thatAny match {
      case that: Foo =>
        (this eq that) || this.a == that.a && this.b == that.b && this.s == that.s && this.i == that.i
      case _ =>
        false
    }

    override def hashCode: _root_.scala.Int = a.hashCode + 13 * (b.hashCode + 13 * (s.hashCode + 13 * i.hashCode))
    override def toString: _root_.java.lang.String = "Foo(" + (a.toString + "," + b.toString + "," + s.toString + "," + i.toString) + ")"

    def copy(a: Option[Boolean] = this.a, b: Option[Boolean] = this.b, s: Option[String] = this.s, i: Option[Int] = this.i): Foo = Foo(a, b, s, i)
    @_root_.scala.throws[_root_.java.io.IOException]
    private[this] def writeObject(out: _root_.java.io.ObjectOutputStream): _root_.scala.Unit = {
      out.writeObject(a)
      out.writeObject(b)
      out.writeObject(s)
      out.writeObject(i)
    }

    @_root_.scala.throws[_root_.java.io.IOException]
    @_root_.scala.throws[_root_.java.lang.ClassNotFoundException]
    private[this] def readObject(in: _root_.java.io.ObjectInputStream): _root_.scala.Unit = {
      val a = in.readObject().asInstanceOf[Option[Boolean]]
      val b = in.readObject().asInstanceOf[Option[Boolean]]
      val s = in.readObject().asInstanceOf[Option[String]]
      val i = in.readObject().asInstanceOf[Option[Int]]
      val packed = Foo.pack(a, b, s, i)
      _s = packed._1
      _i = packed._2
      _bitmask = packed._3
    }

    @_root_.scala.throws[_root_.java.io.ObjectStreamException]
    private[this] def readResolve(): _root_.scala.Any = Foo(a, b, s, i)
    def a: Option[Boolean] = if ((_bitmask & 1 << 0) != 0) {
      _root_.scala.None
    } else {
      _root_.scala.Some((_bitmask & 1 << 1) != 0)
    }
    def b: Option[Boolean] = if ((_bitmask & 1 << 2) != 0) {
      _root_.scala.None
    } else {
      _root_.scala.Some((_bitmask & 1 << 3) != 0)
    }
    def s: Option[String] = if (this._s == null) {
      _root_.scala.None
    } else {
      _root_.scala.Some(new String(this._s))
    }
    def i: Option[Int] = if ((_bitmask & 1 << 4) != 0) {
      _root_.scala.None
    } else {
      _root_.scala.Some(this._i)
    }
  }

  object Foo extends _root_.scala.Serializable {
    def unapply(that: Foo): _root_.scala.Option[(Option[Boolean], Option[Boolean], Option[String], Option[Int])] = _root_.scala.Some((that.a, that.b, that.s, that.i))
    override def toString: _root_.java.lang.String = "Foo"

    @_root_.scala.throws[_root_.java.io.IOException]
    private[this] def writeObject(out: _root_.java.io.ObjectOutputStream): _root_.scala.Unit = ()
    @_root_.scala.throws[_root_.java.io.IOException]
    @_root_.scala.throws[_root_.java.lang.ClassNotFoundException]
    private[this] def readObject(in: _root_.java.io.ObjectInputStream): _root_.scala.Unit = ()
    @_root_.scala.throws[_root_.java.io.ObjectStreamException]
    private[this] def readResolve(): _root_.scala.Any = Foo

    import _root_.shapeless.{ ::, HNil, Generic, LabelledGeneric, Typeable }
    import _root_.shapeless.labelled.{ FieldType, field }
    import _root_.shapeless.syntax.singleton._

    val a_tpe = 'a.narrow
    val b_tpe = 'b.narrow
    val s_tpe = 's.narrow
    val i_tpe = 'i.narrow

    implicit def TypeableFoo(implicit T0: Typeable[Option[String]], T1: Typeable[Option[Int]], T2: Typeable[Option[Boolean]]): Typeable[Foo] = new Typeable[Foo] {
      override def cast(t: _root_.scala.Any): _root_.scala.Option[Foo] = {
        import _root_.shapeless.TypeCase
        val TC0 = TypeCase[Option[String]]
        val TC1 = TypeCase[Option[Int]]
        val TC2 = TypeCase[Option[Boolean]]
        t match {
          case f @ Foo(TC2(a), TC2(b), TC0(s), TC1(i)) =>
            _root_.scala.Some(Foo(a, b, s, i))
          case _ =>
            _root_.scala.None
        }
      }
      override def describe: _root_.java.lang.String = "Foo[" + (T2.describe + "," + T2.describe + "," + T0.describe + "," + T1.describe) + "]"
    }

    implicit val GenericFoo: Generic.Aux[Foo, Option[Boolean] :: Option[Boolean] :: Option[String] :: Option[Int] :: HNil] = new Generic[Foo] {
      override type Repr = Option[Boolean] :: Option[Boolean] :: Option[String] :: Option[Int] :: HNil
      override def to(f: Foo): Repr = LabelledGenericFoo.to(f)
      override def from(r: Repr): Foo = r match {
        case a :: b :: s :: i :: HNil =>
          Foo(a, b, s, i)
      }
    }

    implicit val LabelledGenericFoo: LabelledGeneric.Aux[Foo, FieldType[a_tpe.type, Option[Boolean]] :: FieldType[b_tpe.type, Option[Boolean]] :: FieldType[s_tpe.type, Option[String]] :: FieldType[i_tpe.type, Option[Int]] :: HNil] = new LabelledGeneric[Foo] {
      override type Repr = FieldType[a_tpe.type, Option[Boolean]] :: FieldType[b_tpe.type, Option[Boolean]] :: FieldType[s_tpe.type, Option[String]] :: FieldType[i_tpe.type, Option[Int]] :: HNil
      override def to(f: Foo): Repr = field[a_tpe.type](f.a) :: field[b_tpe.type](f.b) :: field[s_tpe.type](f.s) :: field[i_tpe.type](f.i) :: HNil
      override def from(r: Repr): Foo = GenericFoo.from(r)
    }

    def apply(a: Option[Boolean], b: Option[Boolean], s: Option[String], i: Option[Int]): Foo = {
      val packed = pack(a, b, s, i)
      val created = new Foo(packed._1, packed._2, packed._3)
      created.synchronized(created)
    }

    private def pack(a: Option[Boolean], b: Option[Boolean], s: Option[String], i: Option[Int]): (_root_.scala.Array[_root_.scala.Byte], Int, _root_.scala.Long) = {
      var _bitmask: _root_.scala.Long = 0L
      if (a == _root_.scala.None) {
        _bitmask |= 1 << 0
      } else {
        if (a.get) {
          _bitmask |= 1 << 1
        }
      }
      if (b == _root_.scala.None) {
        _bitmask |= 1 << 2
      } else {
        if (b.get) {
          _bitmask |= 1 << 3
        }
      }
      val _s = if (s == _root_.scala.None) {
        null
      } else {
        s.get.getBytes
      }
      val _i = if (i == _root_.scala.None) {
        _bitmask |= 1 << 4
        0
      } else {
        i.get
      }
      (_s, _i, _bitmask)
    }
  }
}

