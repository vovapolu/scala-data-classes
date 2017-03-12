package testing.memoised

// @data(memoise = true, memoiseStrings = true, memoiseHashCode = true, memoiseToString = true)
// class Foo(a: Boolean, s: String)
final class Foo private (
  private[this] var _a: Boolean,
  private[this] var _s: String,
  // key is used in the memoisation cache (stops it being GCd)
  private[this] val _key: (Boolean, String)
) extends Serializable {

  def a: Boolean = _a
  def s: String = _s

  def copy(a: Boolean = a, s: String = s): Foo = Foo(a, s)

  override val toString: String = s"Foo($a,$s)"
  override val hashCode: Int = a.hashCode + 13 * s.hashCode
  override def equals(o: Any): Boolean = o match {
    //case null                      => false
    case that: Foo if this eq that => true // because of memoisation!

    // can't seem to get guarantees out of the JVM, so need a fallback check
    // case that: Foo if hashCode == that.hashCode => a == that.a && s == that.s

    // DEBUGGING
    case that: Foo if a == that.a && s == that.s =>
      throw new IllegalStateException(s"broken memoisation")

    case _ => false
  }

  @throws[java.io.IOException]
  private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = {
    out.writeBoolean(a)
    out.writeUTF(s)
  }
  @throws[java.io.IOException]
  @throws[ClassNotFoundException]
  private[this] def readObject(in: java.io.ObjectInputStream): Unit = {
    // these will be memoised by readResolve
    _a = in.readBoolean()
    _s = in.readUTF()
  }
  @throws[java.io.ObjectStreamException]
  private[this] def readResolve(): Any = Foo(a, s)

}

final object Foo extends ((Boolean, String) => Foo) {
  override def toString = "Foo"

  // incase somebody serialises the companion (it happens!)
  @throws[java.io.IOException]
  private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = ()
  @throws[java.io.IOException]
  @throws[ClassNotFoundException]
  private[this] def readObject(in: java.io.ObjectInputStream): Unit = ()
  @throws[java.io.ObjectStreamException]
  private[this] def readResolve(raw: Foo.type): Any = Foo

  // WeakHashMap is not ideal for performance. What we really want is
  // a non-blocking WeakHashSet[Foo] that takes a custom Equality.
  private[this] val memoised_cache = new java.util.WeakHashMap[(Boolean, String), java.lang.ref.WeakReference[Foo]]

  // notes on this impl:
  // - putIfAbsent doesn't have the correct semantics for WeakReference values

  // this is an alternative to String.intern, which uses a global cache
  // (we might want to make memoizeStringsIntern an option, sometimes useful)
  private[this] val memoisedStrings_cache = new java.util.WeakHashMap[String, java.lang.ref.WeakReference[String]]
  private[this] def memoisedStrings(s: String): String = {
    // double-checked locking
    val first = {
      val weak = memoisedStrings_cache.get(s)
      if (weak == null) null else weak.get
    }
    if (first != null) first else memoisedStrings_cache.synchronized {
      val got = {
        val weak = memoisedStrings_cache.get(s)
        if (weak == null) null else weak.get
      }
      if (got != null) got else {
        memoisedStrings_cache.put(s, new java.lang.ref.WeakReference(s))
        s
      }
    }
  }

  def apply(a: Boolean, s: String): Foo = {
    val s_cached = memoisedStrings(s)
    val key = (a, s_cached)

    // double-checked locking
    val first = {
      val weak = memoised_cache.get(key)
      if (weak == null) null else weak.get
    }

    if (first != null) first else memoised_cache.synchronized {
      val got = {
        val weak = memoised_cache.get(key)
        if (weak == null) null else {
          val ref = weak.get
          if (ref == null) // DEBUGGING
            println(s"MISS Foo($a, $s) went cold")
          ref
        }
      }
      if (got != null) got else {
        println(s"CREATING Foo($a (${System.identityHashCode(a)}), $s_cached (${System.identityHashCode(s_cached)})), in cache: ${memoised_cache.keySet()}")
        val created = new Foo(a, s_cached, key)
        // safe publication (we have vars, for serialisation)
        val foo = created.synchronized(created)
        memoised_cache.put(key, new java.lang.ref.WeakReference(created))
        foo
      }
    }
  }
  def unapply(f: Foo): Option[(Boolean, String)] = Some((f.a, f.s))

  import shapeless.{::, HNil, Generic, LabelledGeneric, Typeable}
  import shapeless.labelled.{FieldType, field}
  import shapeless.syntax.singleton._
  val a_tpe = 'a.narrow
  val s_tpe = 's.narrow
  implicit val TypeableFoo: Typeable[Foo] =
    new Typeable[Foo] {
      override def cast(t: Any): Option[Foo] = t match {
        case f: Foo => Some(f) // no type params, so trivial
        case _      => None
      }
      override def describe: String = s"Foo[Boolean,String]"
    }

  implicit val LabelledGenericFoo: LabelledGeneric.Aux[Foo, FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: HNil] =
    new LabelledGeneric[Foo] {
      override type Repr = FieldType[a_tpe.type, Boolean] :: FieldType[s_tpe.type, String] :: HNil
      override def to(f: Foo): Repr = field[a_tpe.type](f.a) :: field[s_tpe.type](f.s) :: HNil
      override def from(r: Repr): Foo = GenericFoo.from(r)
    }

  implicit val GenericFoo: Generic.Aux[Foo, Boolean :: String :: HNil] =
    new Generic[Foo] {
      override type Repr = Boolean :: String :: HNil
      override def to(f: Foo): Repr = LabelledGenericFoo.to(f)
      override def from(r: Repr): Foo = r match {
        case a :: s :: HNil => Foo(a, s)
      }
    }

}
