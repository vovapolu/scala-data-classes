//product serializable shapeless companionExtends
class A(a: Boolean, s: String = "a", o: Option[Double])
//---
{
  final class A private (private[this] val _a: Boolean, private[this] val _s: String, private[this] val _o: Option[Double]) extends _root_.scala.Product with _root_.scala.Serializable {

    def a: Boolean = this._a
    def s: String = this._s
    def o: Option[Double] = this._o

    override def equals(thatAny: _root_.scala.Any): _root_.scala.Boolean = thatAny match {
      case that: A =>
        (this eq that) || this.a == that.a && this.s == that.s && this.o == that.o
      case _ =>
        false
    }

    override def hashCode: _root_.scala.Int = a.hashCode + 13 * (s.hashCode + 13 * o.hashCode)
    override def toString: _root_.java.lang.String = "A(" + (a.toString + "," + s.toString + "," + o.toString) + ")"

    def copy(a: Boolean = this.a, s: String = this.s, o: Option[Double] = this.o): A = A(a, s, o)
    def canEqual(that: _root_.scala.Any): _root_.scala.Boolean = that.isInstanceOf[A]

    def productArity: _root_.scala.Int = 3
    def productElement(n: _root_.scala.Int): _root_.scala.Any = n match {
      case 0 =>
        this.a
      case 1 =>
        this.s
      case 2 =>
        this.o
      case _ =>
        throw new _root_.java.lang.IndexOutOfBoundsException(n.toString())
    }
    override def productPrefix: _root_.java.lang.String = "A"
    override def productIterator: _root_.scala.Iterator[_root_.scala.Any] = _root_.scala.runtime.ScalaRunTime.typedProductIterator[_root_.scala.Any](this)

    @_root_.scala.throws[_root_.java.io.ObjectStreamException]
    private[this] def readResolve(): _root_.scala.Any = A(a, s, o)
  }

  object A extends _root_.scala.Serializable with ((Boolean, String, Option[Double]) => A) {

    def apply(a: Boolean, s: String = "a", o: Option[Double]): A = {
      val created = new A(a, s, o)
      created
    }

    def unapply(that: A): _root_.scala.Option[(Boolean, String, Option[Double])] = _root_.scala.Some((that.a, that.s, that.o))
    override def toString: _root_.java.lang.String = "A"

    @_root_.scala.throws[_root_.java.io.IOException]
    private[this] def writeObject(out: _root_.java.io.ObjectOutputStream): _root_.scala.Unit = ()
    @_root_.scala.throws[_root_.java.io.IOException]
    @_root_.scala.throws[_root_.java.lang.ClassNotFoundException]
    private[this] def readObject(in: _root_.java.io.ObjectInputStream): _root_.scala.Unit = ()
    @_root_.scala.throws[_root_.java.io.ObjectStreamException]
    private[this] def readResolve(): _root_.scala.Any = A

    import _root_.shapeless.{ ::, HNil, Generic, LabelledGeneric, Typeable }
    import _root_.shapeless.labelled.{ FieldType, field }
    import _root_.shapeless.syntax.singleton._

    val a_tpe = 'a.narrow
    val s_tpe = 's.narrow
    val o_tpe = 'o.narrow

    implicit def TypeableA(implicit T0: Typeable[Option[Double]]): Typeable[A] = new Typeable[A] {
      override def cast(t: _root_.scala.Any): _root_.scala.Option[A] = {
        import _root_.shapeless.TypeCase
        val TC0 = TypeCase[Option[Double]]
        t match {
          case f @ A(a, s, TC0(o)) =>
            _root_.scala.Some(A(a, s, o))
          case _ =>
            _root_.scala.None
        }
      }
      override def describe: _root_.java.lang.String = "A[" + ("Boolean" + "," + "String" + "," + T0.describe) + "]"
    }

    implicit val GenericA: Generic.Aux[A, Boolean :: String :: Option[Double] :: HNil] = new Generic[A] {
      override type Repr = Boolean :: String :: Option[Double] :: HNil
      override def to(f: A): Repr = LabelledGenericA.to(f)
      override def from(r: Repr): A = r match {
        case a :: s :: o :: HNil =>
          A(a, s, o)
      }
    }

    implicit val LabelledGenericA: LabelledGeneric.Aux[A, FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: FieldType[o_tpe.type, Option[Double]] :: HNil] = new LabelledGeneric[A] {
      override type Repr = FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: FieldType[o_tpe.type, Option[Double]] :: HNil
      override def to(f: A): Repr = field[a_tpe.type](f.a) :: field[s_tpe.type](f.s) :: field[o_tpe.type](f.o) :: HNil
      override def from(r: Repr): A = GenericA.from(r)
    }
  }
}



