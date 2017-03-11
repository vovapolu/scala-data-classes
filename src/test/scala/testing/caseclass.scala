package testing.caseclass

// example of feature parity with final case class

// @data(product = true, checkSerializable = false)
// class Foo[+T](a: Boolean, s: String, t: T, i: Int = 0)
final class Foo[+T] private (
  private[this] val _a: Boolean,
  private[this] val _s: String,
  private[this] val _t: T,
  private[this] val _i: Int
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
    case f: Foo[_] => a == f.a && s == f.s && t == f.t && i == f.i
    case _         => false
  }

  // should go via the companion to force whatever logic we put there
  private[this] def readResolve(): Foo[T] = Foo(a, s, t, i)

}

// I'm not sure how to extend Function when there are type parameters...
final object Foo {
  override def toString = "Foo"

  // incase somebody serialises the companion (it happens!)
  private[this] def readResolve(raw: Foo.type): Foo.type = Foo

  // note, default value on `i`
  def apply[T](a: Boolean, s: String, t: T, i: Int = 0): Foo[T] = new Foo(a, s, t, i)
  def unapply[T](f: Foo[T]): Option[(Boolean, String, T, Int)] = Some((f.a, f.s, f.t, f.i))

  // if there are no type parameters on the class, this can be a val
  // (*sigh* if only scala had language support for singleton symbols...)
  import shapeless.{::, HNil, Generic, LabelledGeneric}
  import shapeless.labelled.{FieldType, field}
  import shapeless.syntax.singleton._
  // it might be possible to avoid these tagged symbols inside scala.meta
  val a_tpe = 'a.narrow
  val s_tpe = 's.narrow
  val t_tpe = 't.narrow
  val i_tpe = 'i.narrow
  type LabelledGenericRepr[T] = FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: FieldType[t_tpe.type, T] :: FieldType[i_tpe.type, Int] :: HNil
  type GenericRepr[T] = Boolean :: String :: T :: Int :: HNil

  implicit def GenericFoo[T]: Generic.Aux[Foo[T], GenericRepr[T]] =
    new Generic[Foo[T]] {
      override type Repr = GenericRepr[T]
      override def to(f: Foo[T]): Repr = f.a :: f.s :: f.t :: f.i :: HNil
      override def from(r: Repr): Foo[T] = r match {
        case a :: s :: t :: i :: HNil => Foo(a, s, t, i)
      }
    }

  implicit def LabelledGenericFoo[T]: LabelledGeneric.Aux[Foo[T], LabelledGenericRepr[T]] =
    new LabelledGeneric[Foo[T]] {
      override type Repr = LabelledGenericRepr[T]

      override def to(f: Foo[T]): Repr =
        field[a_tpe.type](f.a) ::
          field[s_tpe.type](f.s) ::
          field[t_tpe.type](f.t) ::
          field[i_tpe.type](f.i) ::
          HNil

      override def from(r: Repr): Foo[T] = r match {
        case a :: s :: t :: i :: HNil => Foo(a, s, t, i)
      }

    }

}
