package testing

// example of feature parity with final case class

// NOTE: currently hard-coded so we can write tests (and as a spec),
//       this should be generated from a (reasonably complex)

// @data class Foo[+T](a: Boolean, s: String, t: T)
final class Foo[+T] private (
  private[this] val _a: Boolean,
  private[this] val _s: String,
  private[this] val _t: T
) extends Serializable {
  // ideally, I'd like Serializable to be checked at compile time

  def a: Boolean = _a
  def s: String = _s
  def t: T = _t

  override def toString(): String = ???
  override def hashCode(): Int = ???
  override def equals(o: Any): Boolean = ???

  // should use the public field accesors, not the internal ones
  private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = ???
  private[this] def readObject(in: java.io.ObjectInputStream): Unit = ???

  // should go via the companion to force whatever logic we put there
  private[this] def readResolve(raw: Foo[_]) = Foo(raw.a, raw.s, raw.t)

}

object Foo /*extends Function3[Boolean, String, T, Foo[T]]*/ {
  // incase somebody serialises the companion (it happens!)
  private[this] def readResolve(raw: Foo.type): Foo.type = Foo

  def apply[T](a: Boolean, s: String, t: T): Foo[T] = ???
  def unapply[T](a: Boolean, s: String, t: T): Option[Foo[T]] = ???

  // if there are no type parameters on the class, this can be a val
  implicit def LabelledGenericFoo[T](t: T): shapeless.LabelledGeneric[Foo[T]] = ???
}
