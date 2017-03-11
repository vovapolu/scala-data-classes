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
  private[this] def readResolve(raw: Foo[_]) = Foo(raw.a, raw.s, raw.t, raw.i)

}

// I'm not sure how to extend Function when there are type parameters...
final object Foo {
  // incase somebody serialises the companion (it happens!)
  private[this] def readResolve(raw: Foo.type): Foo.type = Foo

  // note, default value on `i`
  def apply[T](a: Boolean, s: String, t: T, i: Int = 0): Foo[T] = ???
  def unapply[T](a: Boolean, s: String, t: T, i: Int): Option[Foo[T]] = ???

  // if there are no type parameters on the class, this can be a val
  implicit def LabelledGenericFoo[T](t: T): shapeless.LabelledGeneric[Foo[T]] = ???
}
