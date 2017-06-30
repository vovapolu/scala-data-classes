//product serializable shapeless
class A(a: Boolean, s: String = "a")
//---
{
  final class A private (private[this] var _a: Boolean, private[this] var _s: String) extends Product with Serializable {
    def a: Boolean = this._a
    def s: String = this._s

    override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object]) || (thatAny match {
      case that: A =>
        that.a == this.a && that.s == this.s
      case _ =>
        false
    })

    override def hashCode(): Int = a.hashCode + 13 * s.hashCode
    override def toString: String = "A(" + (a.toString + "," + s.toString) + ")"
    def copy(a: Boolean = this.a, s: String = this.s): A = new A(a, s)

    @throws[_root_.java.io.IOException]
    private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = {
      out.writeBoolean(a)
      out.writeUTF(s)
    }
    @throws[_root_.java.io.IOException]
    @throws[_root_.java.lang.ClassNotFoundException]
    private[this] def readObject(in: java.io.ObjectInputStream): Unit = {
      _a = in.readBoolean()
      _s = in.readUTF()
    }
    @throws[_root_.java.io.ObjectStreamException]
    private[this] def readResolve(): Any = A(a, s)

    def canEqual(that: Any): Boolean = that.isInstanceOf[A]
    def productArity: Int = 2
    def productElement(n: Int): Any = n match {
      case 0 =>
        this.a
      case 1 =>
        this.s
      case _ =>
        throw new IndexOutOfBoundsException(n.toString())
    }
    override def productPrefix: String = "A"
    override def productIterator: Iterator[Any] = scala.runtime.ScalaRunTime.typedProductIterator[Any](this)
  }

  object A extends Serializable {
    def apply(a: Boolean, s: String = "a"): A = {
      val newVal = new A(a, s)
      newVal.synchronized(newVal)
    }

    def unapply(that: A): Option[(Boolean, String)] = Some((that.a, that.s))

    override def toString: String = "A"

    @throws[_root_.java.io.IOException]
    private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = ()

    @throws[_root_.java.io.IOException]
    @throws[_root_.java.lang.ClassNotFoundException]
    private[this] def readObject(in: java.io.ObjectInputStream): Unit = ()

    @throws[_root_.java.io.ObjectStreamException]
    private[this] def readResolve(): Any = A

    import _root_.shapeless.{ ::, HNil, Generic, LabelledGeneric, Typeable, TypeCase }
    import _root_.shapeless.labelled.{ FieldType, field }
    import _root_.shapeless.syntax.singleton._

    val a_tpe = Symbol("a").narrow
    val s_tpe = Symbol("s").narrow

    implicit def TypeableA(): Typeable[A] = new Typeable[A] {
      override def cast(t: Any): Option[A] = {
        t match {
          case f @ A(a, s) => Some(A(a, s))
          case _ => None
        }
      }
      override def describe: String = "A[" + ("Boolean" + "," + "String") + "]"
    }

    implicit def GenericA: Generic.Aux[A, Boolean :: String :: HNil] = new Generic[A] {
      override type Repr = Boolean :: String :: HNil
      override def to(f: A): Repr = LabelledGenericA.to(f)
      override def from(r: Repr): A = r match {
        case a :: s :: HNil =>
          A(a, s)
      }
    }

    implicit def LabelledGenericA: LabelledGeneric.Aux[A,
        FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: HNil] = new LabelledGeneric[A] {
      override type Repr = FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: HNil
      override def to(f: A): Repr = field[a_tpe.type](f.a) :: field[s_tpe.type](f.s) :: HNil
      override def from(r: Repr): A = GenericA.from(r)
    }

  }
}

