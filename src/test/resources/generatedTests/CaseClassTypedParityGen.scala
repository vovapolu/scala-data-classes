//product serializable shapeless
class A[T](a: Boolean, s: String, t: T)
//---
{
  final class A[T] private(private[this] var _a: Boolean, private[this] var _s: String, private[this] var _t: T) extends _root_.scala.Product with _root_.scala.Serializable {

    import _root_.scala._
    import _root_.scala.Predef._

    def a: Boolean = this._a
    def s: String = this._s
    def t: T = this._t

    override def equals(thatAny: Any): Boolean = thatAny match {
      case that: A[_] =>
        (this eq that) || that.a == this.a && that.s == this.s && that.t == this.t
      case _ =>
        false
    }

    override def hashCode(): Int = a.hashCode + 13 * (s.hashCode + 13 * t.hashCode)
    override def toString: String = "A(" + (a.toString + "," + s.toString + "," + t.toString) + ")"

    def copy[N$T >: T](a: Boolean = this.a, s: String = this.s, t: N$T = this.t): A[N$T] = A(a, s, t)

    def canEqual(that: Any): Boolean = that.isInstanceOf[A[T]]

    def productArity: Int = 3
    def productElement(n: Int): Any = n match {
      case 0 =>
        this.a
      case 1 =>
        this.s
      case 2 =>
        this.t
      case _ =>
        throw new IndexOutOfBoundsException(n.toString())
    }
    override def productPrefix: String = "A"
    override def productIterator: Iterator[Any] = scala.runtime.ScalaRunTime.typedProductIterator[Any](this)

    @throws[_root_.java.io.IOException]
    private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = {
      out.writeBoolean(a)
      out.writeUTF(s)
      out.writeObject(t)
    }

    @throws[_root_.java.io.IOException]
    @throws[_root_.java.lang.ClassNotFoundException]
    private[this] def readObject(in: java.io.ObjectInputStream):
    Unit = {
      _a = in.readBoolean()
      _s = in.readUTF()
      _t = in.readObject().asInstanceOf[T]
    }

    @throws[_root_.java.io.ObjectStreamException]
    private[this] def readResolve(): Any = A(a, s, t)
  }

  object A extends _root_.scala.Serializable {

    import _root_.scala._
    import _root_.scala.Predef._

    def apply[T](a: Boolean, s: String, t: T): A[T] = {
      val newVal = new A(a, s, t)
      newVal.synchronized(newVal)
    }

    def unapply[T](that: A[T]): Option[(Boolean, String, T)] = Some((that.a, that.s, that.t))

    override def toString: String = "A"

    @throws[_root_.java.io.IOException]
    private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = ()

    @throws[_root_.java.io.IOException]
    @throws[_root_.java.lang.ClassNotFoundException]
    private[this] def readObject(in: java.io.ObjectInputStream): Unit = ()

    @throws[_root_.java.io.ObjectStreamException]
    private[this] def readResolve(): Any = A

    import _root_.shapeless.{::, HNil, Generic, LabelledGeneric, Typeable, TypeCase}
    import _root_.shapeless.labelled.{FieldType, field}
    import _root_.shapeless.syntax.singleton._

    val a_tpe = Symbol("a").narrow
    val s_tpe = Symbol("s").narrow
    val t_tpe = Symbol("t").narrow

    implicit def TypeableA[T](implicit TT: Typeable[T]): Typeable[A[T]] = new Typeable[A[T]] {
      override def cast(t: Any): Option[A[T]] = {
        val TC_T = TypeCase[T]
        t match {
          case f@A(a, s, TC_T(t)) =>
            Some(A(a, s, t))
          case _ =>
            None
        }
      }

      override def describe: String = "A[" + ("Boolean" + "," + "String" + "," + TT.describe) + "]"
    }

    implicit def GenericA[T]: Generic.Aux[A[T], Boolean :: String :: T :: HNil] = new Generic[A[T]] {
      override type Repr = Boolean :: String :: T :: HNil

      override def to(f: A[T]): Repr = LabelledGenericA[T].to(f)

      override def from(r: Repr): A[T] = r match {
        case a :: s :: t :: HNil =>
          A(a, s, t)
      }
    }

    implicit def LabelledGenericA[T]: LabelledGeneric.Aux[A[T], FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: FieldType[t_tpe.type, T] :: HNil] = new LabelledGeneric[A[T]] {
      override type Repr = FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: FieldType[t_tpe.type, T] :: HNil

      override def to(f: A[T]): Repr = field[a_tpe.type](f.a) :: field[s_tpe.type](f.s) :: field[t_tpe.type](f.t) :: HNil

      override def from(r: Repr): A[T] = GenericA[T].from(r)
    }
  }
}