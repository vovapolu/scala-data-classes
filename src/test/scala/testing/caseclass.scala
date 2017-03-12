package testing.caseclass

// example of feature parity with final case class

// @data(product = true, checkSerializable = false)
// class Foo[+T](a: Boolean, s: String, t: T, i: Int = 0)
final class Foo[+T] private (
  private[this] var _a: Boolean,
  private[this] var _s: String,
  private[this] var _t: T,
  private[this] var _i: Int
) extends Serializable with Product {
  def a: Boolean = _a
  def s: String = _s
  def t: T = _t
  def i: Int = _i

  def copy[S >: T](a: Boolean = a, s: String = s, t: S = t, i: Int = i): Foo[S] = Foo(a, s, t, i)

  override def productPrefix = "Foo"
  override def productArity: Int = 4
  override def productElement(n: Int): Any = (n: @scala.annotation.switch) match {
    case 0 => a
    case 1 => s
    case 2 => t
    case 3 => i
  }
  override def canEqual(o: Any): Boolean = o != null && o.isInstanceOf[Foo[_]]

  override def toString(): String = s"Foo($a,$s,$t,$i)"
  override def hashCode(): Int = a.hashCode + 13 * (s.hashCode + 13 * (t.hashCode + 13 * i.hashCode))
  override def equals(o: Any): Boolean = o match {
    case that: Foo[_] => (this eq that) || (a == that.a && s == that.s && t == that.t && i == that.i)
    case _            => false
  }

  @throws[java.io.IOException]
  private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.writeBoolean(a)
    out.writeUTF(s)
    out.writeObject(t) // NOTE: not checking Serializable
    out.writeInt(i)
  }
  @throws[java.io.IOException]
  @throws[ClassNotFoundException]
  private[this] def readObject(in: java.io.ObjectInputStream): Unit = {
    _a = in.readBoolean()
    _s = in.readUTF()
    _t = in.readObject().asInstanceOf[T]
    _i = in.readInt
  }
  // readObjectNoData not needed because class is final
  @throws[java.io.ObjectStreamException]
  private[this] def readResolve(): Any = Foo(a, s, t, i)

}

// companionExtends would try to add `extends ((...) => Foo)` for non-parametric classes
final object Foo extends Serializable {
  override def toString = "Foo"

  // incase somebody serialises the companion (it happens!)
  @throws[java.io.IOException]
  private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = ()
  @throws[java.io.IOException]
  @throws[ClassNotFoundException]
  private[this] def readObject(in: java.io.ObjectInputStream): Unit = ()
  @throws[java.io.ObjectStreamException]
  private[this] def readResolve(raw: Foo.type): Any = Foo

  // note, default value on `i`
  def apply[T](a: Boolean, s: String, t: T, i: Int = 0): Foo[T] = {
    val foo = new Foo(a, s, t, i)
    foo.synchronized(foo) // safe publication (we have vars, for serialisation)
  }
  def unapply[T](f: Foo[T]): Option[(Boolean, String, T, Int)] = Some((f.a, f.s, f.t, f.i))

  // if there are no type parameters on the class, this can be a val
  // (*sigh* if only scala had language support for singleton symbols...)
  import shapeless.{::, HNil, Generic, LabelledGeneric, Typeable, TypeCase}
  import shapeless.labelled.{FieldType, field}
  import shapeless.syntax.singleton._
  // it might be possible to avoid these tagged symbols inside scala.meta
  val a_tpe = 'a.narrow
  val s_tpe = 's.narrow
  val t_tpe = 't.narrow
  val i_tpe = 'i.narrow

  implicit def TypeableFoo[T](implicit tt: Typeable[T]): Typeable[Foo[T]] =
    new Typeable[Foo[T]] {
      override def cast(t: Any): Option[Foo[T]] = {
        val TC_T = TypeCase[T]
        t match {
          // create a new one to be safe (maybe too safe? ask Miles...)
          case f @ Foo(b, s, TC_T(t), i) => Some(Foo(b, s, t, i))
          case _                         => None
        }
      }
      override def describe: String = s"Foo[Boolean,String,${tt.describe},Int]"
    }

  implicit def LabelledGenericFoo[T]: LabelledGeneric.Aux[Foo[T], FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: FieldType[t_tpe.type, T] :: FieldType[i_tpe.type, Int] :: HNil] =
    new LabelledGeneric[Foo[T]] {
      override type Repr = FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: FieldType[t_tpe.type, T] :: FieldType[i_tpe.type, Int] :: HNil

      override def to(f: Foo[T]): Repr =
        field[a_tpe.type](f.a) ::
          field[s_tpe.type](f.s) ::
          field[t_tpe.type](f.t) ::
          field[i_tpe.type](f.i) ::
          HNil

      override def from(r: Repr): Foo[T] = GenericFoo.from(r)
    }

  implicit def GenericFoo[T]: Generic.Aux[Foo[T], Boolean :: String :: T :: Int :: HNil] =
    new Generic[Foo[T]] {
      override type Repr = Boolean :: String :: T :: Int :: HNil
      override def to(f: Foo[T]): Repr = LabelledGenericFoo[T].to(f)
      override def from(r: Repr): Foo[T] = r match {
        case a :: s :: t :: i :: HNil => Foo(a, s, t, i)
      }
    }

}
