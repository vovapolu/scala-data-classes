//product serializable shapeless
class A[X, Y](a: Boolean, s: String, xy: Either[X, Option[Y]])
//---
{
  final class A[X, Y] private (private[this] val _a: Boolean, private[this] val _s: String, private[this] val _xy: Either[X, Option[Y]]) extends _root_.scala.Product with _root_.scala.Serializable {
    import _root_.scala._
    import _root_.scala.Predef._

    def a: Boolean = this._a
    def s: String = this._s
    def xy: Either[X, Option[Y]] = this._xy

    override def equals(thatAny: Any): Boolean = thatAny match {
      case that: A[_, _] =>
        (this eq that) || this.a == that.a && this.s == that.s && this.xy == that.xy
      case _ =>
        false
    }

    override def hashCode: Int = a.hashCode + 13 * (s.hashCode + 13 * xy.hashCode)
    override def toString: String = "A(" + (a.toString + "," + s.toString + "," + xy.toString) + ")"

    def copy[N$X, N$Y](a: Boolean = this.a, s: String = this.s, xy: Either[N$X, Option[N$Y]] = this.xy): A[N$X, N$Y] = A(a, s, xy)

    def canEqual(that: Any): Boolean = that.isInstanceOf[A[X, Y]]
    def productArity: Int = 3
    def productElement(n: Int): Any = n match {
      case 0 =>
        this.a
      case 1 =>
        this.s
      case 2 =>
        this.xy
      case _ =>
        throw new IndexOutOfBoundsException(n.toString())
    }
    override def productPrefix: String = "A"
    override def productIterator: Iterator[Any] = scala.runtime.ScalaRunTime.typedProductIterator[Any](this)

    @throws[_root_.java.io.ObjectStreamException]
    private[this] def readResolve(): Any = A(a, s, xy)
  }

  object A extends _root_.scala.Serializable {
    import _root_.scala._
    import _root_.scala.Predef._

    def apply[X, Y](a: Boolean, s: String, xy: Either[X, Option[Y]]): A[X, Y] = {
      val created = new A(a, s, xy)
      created
    }

    def unapply[X, Y](that: A[X, Y]): Option[(Boolean, String, Either[X, Option[Y]])] = Some((that.a, that.s, that.xy))

    override def toString: String = "A"

    @throws[_root_.java.io.IOException]
    private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = ()
    @throws[_root_.java.io.IOException]
    @throws[_root_.java.lang.ClassNotFoundException]
    private[this] def readObject(in: java.io.ObjectInputStream): Unit = ()
    @throws[_root_.java.io.ObjectStreamException]
    private[this] def readResolve(): Any = A

    import _root_.shapeless.{ ::, HNil, Generic, LabelledGeneric, Typeable }
    import _root_.shapeless.labelled.{ FieldType, field }
    import _root_.shapeless.syntax.singleton._

    val a_tpe = Symbol("a").narrow
    val s_tpe = Symbol("s").narrow
    val xy_tpe = Symbol("xy").narrow

    implicit def TypeableA[X, Y](implicit T0: Typeable[Either[X, Option[Y]]]): Typeable[A[X, Y]] = new Typeable[A[X, Y]] {
      override def cast(t: Any): Option[A[X, Y]] = {
        import _root_.shapeless.TypeCase
        val TC0 = TypeCase[Either[X, Option[Y]]]
        t match {
          case f @ A(a, s, TC0(xy)) =>
            Some(A(a, s, xy))
          case _ =>
            None
        }
      }
      override def describe: String = "A[" + ("Boolean" + "," + "String" + "," + T0.describe) + "]"
    }

    implicit def GenericA[X, Y]: Generic.Aux[A[X, Y], Boolean :: String :: Either[X, Option[Y]] :: HNil] = new Generic[A[X, Y]] {
      override type Repr = Boolean :: String :: Either[X, Option[Y]] :: HNil
      override def to(f: A[X, Y]): Repr = LabelledGenericA[X, Y].to(f)
      override def from(r: Repr): A[X, Y] = r match {
        case a :: s :: xy :: HNil =>
          A(a, s, xy)
      }
    }

    implicit def LabelledGenericA[X, Y]: LabelledGeneric.Aux[A[X, Y], FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: FieldType[xy_tpe.type, Either[X, Option[Y]]] :: HNil] = new LabelledGeneric[A[X, Y]] {
      override type Repr = FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: FieldType[xy_tpe.type, Either[X, Option[Y]]] :: HNil
      override def to(f: A[X, Y]): Repr = field[a_tpe.type](f.a) :: field[s_tpe.type](f.s) :: field[xy_tpe.type](f.xy) :: HNil
      override def from(r: Repr): A[X, Y] = GenericA[X, Y].from(r)
    }
  }
}
