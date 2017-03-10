package testing

// @data(memoize = true, valHashCode = true)
// class Bar(a: Boolean, s: String)
final class Bar private (
  private[this] val _a: Boolean,
  private[this] val _s: String
) extends Serializable {

  def a: Boolean = _a
  def s: String = _s

  override def toString(): String = ???
  override val hashCode: Int = 0
  override def equals(o: Any): Boolean = o match {
    case that: Bar if this eq that => true // because of memoisation!
    case _                         => false
  }

  // should use the public field accesors, not the internal ones
  private def writeObject(out: java.io.ObjectOutputStream): Unit = ???
  private def readObject(in: java.io.ObjectInputStream): Unit = ???

  private def readResolve(raw: Bar) = Bar(raw.a, raw.s)

}

object Bar extends ((Boolean, String) => Bar) {
  private def readResolve(raw: Bar.type) = Bar

  // WeakHashMap is not ideal for performance. What we really want is
  // a non-blocking WeakHashSet[Bar] that takes a custom Equality and
  // "memoize = true" should always generate a val hashCode.
  private val cache: java.util.WeakHashMap[(Boolean, String), Bar] = null

  def apply(a: Boolean, s: String): Bar = synchronized {
    val key = (a, s)
    val got = cache.get(key)
    if (got != null) got
    else {
      val created = new Bar(a, s)
      cache.put(key, created)
      created
    }
  }
  def unapply(a: Boolean, s: String): Option[Bar] = ???

  // no type parameters, so this can be a val
  implicit val LabelledGenericBar: shapeless.LabelledGeneric[Bar] = null
}
