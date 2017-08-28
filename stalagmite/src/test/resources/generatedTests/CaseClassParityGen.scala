//product serializable shapeless companionExtends
class A(a: Boolean, s: String = "a", o: Option[Double])
//---
{
  final class A private (private[this] val _a: Boolean, private[this] val _s: String, private[this] val _o: Option[Double]) extends _root_.scala.Product with _root_.scala.Serializable {
    import _root_.scala._
    import _root_.scala.Predef._

    def a: Boolean = this._a
    def s: String = this._s
    def o: Option[Double] = this._o

    override def equals(thatAny: Any): Boolean = thatAny match {
      case that: A =>
        (this eq that) || this.a == that.a && this.s == that.s && this.o == that.o
      case _ =>
        false
    }

    override def hashCode: Int = a.hashCode + 13 * (s.hashCode + 13 * o.hashCode)
    override def toString: String = "A(" + (a.toString + "," + s.toString + "," + o.toString) + ")"

    def copy(a: Boolean = this.a, s: String = this.s, o: Option[Double] = this.o): A = A(a, s, o)

    def canEqual(that: Any): Boolean = that.isInstanceOf[A]
    def productArity: Int = 3
    def productElement(n: Int): Any = n match {
      case 0 =>
        this.a
      case 1 =>
        this.s
      case 2 =>
        this.o
      case _ =>
        throw new IndexOutOfBoundsException(n.toString())
    }
    override def productPrefix: String = "A"
    override def productIterator: Iterator[Any] = scala.runtime.ScalaRunTime.typedProductIterator[Any](this)

    @throws[_root_.java.io.ObjectStreamException]
    private[this] def readResolve(): Any = A(a, s, o)
  }

  object A extends _root_.scala.Serializable with ((Boolean, String, Option[Double]) => A) {
    import _root_.scala._
    import _root_.scala.Predef._

    def apply(a: Boolean, s: String = "a", o: Option[Double]): A = {
      val created = new A(a, s, o)
      created
    }

    def unapply(that: A): Option[(Boolean, String, Option[Double])] = Some((that.a, that.s, that.o))

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
    val o_tpe = Symbol("o").narrow

    implicit def TypeableA(implicit T0: Typeable[Option[Double]]): Typeable[A] = new Typeable[A] {
      override def cast(t: Any): Option[A] = {
        import _root_.shapeless.TypeCase
        val TC0 = TypeCase[Option[Double]]
        t match {
          case f @ A(a, s, TC0(o)) =>
            Some(A(a, s, o))
          case _ =>
            None
        }
      }
      override def describe: String = "A[" + ("Boolean" + "," + "String" + "," + T0.describe) + "]"
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


