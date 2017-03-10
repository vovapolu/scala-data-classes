package testing

// @data(optimiseRepr = true, optimiseString = true, memoizeStrings = true)
// class Baz(a: Option[Boolean], b: Option[Boolean], s: Option[String])
final class Baz private (
  private[this] val _bitmask: Long,
  private[this] val _s: Array[Char]
// note that this saves 3x Option wrappers, Boolean wrappers,
// redundant Boolean value, String wrapper. Reducing heap storage
// overhead of each instance of Baz by 6 references (384 bits).
) extends Serializable {

  def a: Option[Boolean] = {
    if ((_bitmask & 0x00) != 0) None
    else Some((_bitmask & 0x01) != 0)
  }
  def b: Option[Boolean] = {
    if ((_bitmask & 0x02) != 0) None
    else Some((_bitmask & 0x03) != 0)
  }
  def s: Option[String] = {
    if ((_bitmask & 0x04) != 0) None
    else if ((_bitmask & 0x05) != 0) Some(null)
    else Some(new String(_s)) // optimiseString
  }

  override def toString(): String = ???
  override def hashCode: Int = ???
  override def equals(o: Any): Boolean = ??? // can't use eq for _s when optimiseString is used

  // should use the public field accesors, not the internal ones
  private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = ???
  private[this] def readObject(in: java.io.ObjectInputStream): Unit = ???

  private[this] def readResolve(raw: Baz) = Baz(raw.a, raw.b, raw.s)

}

object Baz extends ((Option[Boolean], Option[Boolean], Option[String]) => Baz) {
  private[this] def readResolve(raw: Baz.type): Baz.type = Baz

  // optional heap optimisation: string fields are memoized
  private[this] val string_cache: java.util.WeakHashMap[String, Boolean] = null

  def apply(a: Option[Boolean], b: Option[Boolean], s: Option[String]): Baz = ???
  def unapply(a: Option[Boolean], b: Option[Boolean], s: Option[String]): Option[Baz] = ???

  // no type parameters, so this can be a val
  implicit val LabelledGenericBaz: shapeless.LabelledGeneric[Baz] = null
}
