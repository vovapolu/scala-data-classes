package testing.memoised

// @data(memoise = true, memoiseStrings = true, memoiseHashCode = true, memoiseToString = true)
// class Foo(a: Boolean, s: String)
final class Foo private (
  private[this] val _a: Boolean,
  private[this] val _s: String
) extends Serializable {

  def a: Boolean = _a
  def s: String = _s

  def copy(a: Boolean = a, s: String = s): Foo = ???

  override val toString: String = null
  override val hashCode: Int = 0
  override def equals(o: Any): Boolean = o match {
    case that: Foo if this eq that => true // because of memoisation!
    case _                         => false
  }

  private[this] def readResolve(): Foo = Foo(a, s)

}

final object Foo extends ((Boolean, String) => Foo) {
  private[this] def readResolve(raw: Foo.type): Foo.type = Foo

  // WeakHashMap is not ideal for performance. What we really want is
  // a non-blocking WeakHashSet[Foo] that takes a custom Equality and
  // "memoise = true" should always generate a val hashCode.
  private[this] val cache: java.util.WeakHashMap[(Boolean, String), Foo] = null

  // optional heap optimisation: string fields are memoised
  private[this] val string_cache: java.util.WeakHashMap[String, Boolean] = null

  def apply(a: Boolean, s: String): Foo = synchronized {
    val s_cached = s // TODO: obtain memoised s
    val key = (a, s_cached)
    val got = cache.get(key)
    if (got != null) got
    else {
      val created = new Foo(a, s_cached)
      cache.put(key, created)
      created
    }
  }
  def unapply(f: Foo): Option[(Boolean, String)] = ???

  // no type parameters, so this can be a val
  implicit val LabelledGenericFoo: shapeless.LabelledGeneric[Foo] = null
}
