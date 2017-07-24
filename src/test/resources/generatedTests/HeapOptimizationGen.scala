//optimiseHeapOptions optimiseHeapBooleans optimiseHeapStrings memoise serializable
//memoiseRefs s
class Foo(a: Option[Boolean], b: Option[Boolean], s: Option[String])
//---
{
  final class Foo private (private[this] var _s: Array[Byte], private[this] var _bitmask: Long) extends _root_.scala.Serializable {
    import _root_.scala._
    import _root_.scala.Predef._

    def a: Option[Boolean] = if ((_bitmask & 1 << 0) != 0) {
      None
    } else {
      (_bitmask & 1 << 1) != 0
    }

    def b: Option[Boolean] = if ((_bitmask & 1 << 2) != 0) {
      None
    } else {
      (_bitmask & 1 << 3) != 0
    }

    def s: Option[String] = if (this._s == null) {
      None
    } else {
      Some(new String(this._s))
    }

    override def equals(thatAny: Any): Boolean = thatAny match {
      case that: Foo =>
        (this eq that) || that.a == this.a && that.b == this.b && that.s == this.s
      case _ =>
        false
    }

    override def hashCode: Int = a.hashCode + 13 * (b.hashCode + 13 * s.hashCode)
    override def toString: String = "Foo(" + (a.toString + "," + b.toString + "," + s.toString) + ")"

    def copy(a: Option[Boolean] = this.a, b: Option[Boolean] = this.b, s: Option[String] = this.s): Foo = Foo(a, b, s)

    @throws[_root_.java.io.IOException]
    private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = {
      out.writeObject(a)
      out.writeObject(b)
      out.writeObject(s)
    }

    @throws[_root_.java.io.IOException]
    @throws[_root_.java.lang.ClassNotFoundException]
    private[this] def readObject(in: java.io.ObjectInputStream): Unit = {
      val a = in.readObject().asInstanceOf[Option[Boolean]]
      val b = in.readObject().asInstanceOf[Option[Boolean]]
      val s = in.readObject().asInstanceOf[Option[String]]
      val packed = Foo.pack(a, b, s)
      _s = packed._0
      _bitmask = packed._1
    }
    @throws[_root_.java.io.ObjectStreamException]
    private[this] def readResolve(): Any = Foo(a, b, s)

    def intern: Foo = Foo(a, b, s)
  }

  object Foo extends _root_.scala.Serializable {
    import _root_.scala._
    import _root_.scala.Predef._

    def apply(a: Option[Boolean], b: Option[Boolean], s: Option[String]): Foo = {
      val s_memoised = memoisedRef_cache.intern(s).asInstanceOf[Option[String]]
      val packed = pack(a, b, s_memoised)
      val created = new Foo(packed._0, packed._1)
      val safe = created.synchronized(created)
      memoised_cache.intern(safe)
    }

    def unapply(that: Foo): Option[(Option[Boolean], Option[Boolean], Option[String])] = Some((that.a, that.b, that.s))
    override def toString: String = "Foo"

    @throws[_root_.java.io.IOException]
    private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = ()
    @throws[_root_.java.io.IOException]
    @throws[_root_.java.lang.ClassNotFoundException]
    private[this] def readObject(in: java.io.ObjectInputStream): Unit = ()
    @throws[_root_.java.io.ObjectStreamException]
    private[this] def readResolve(): Any = Foo

    private[this] val memoised_cache = _root_.com.google.common.collect.Interners.newWeakInterner[Foo]()
    private[this] val memoisedRef_cache = _root_.com.google.common.collect.Interners.newWeakInterner[AnyRef]()

    private def pack(a: Option[Boolean], b: Option[Boolean], s: Option[String]): (Array[Byte], Long) = {
      var _bitmask: Long = 0L
      if (a == None) {
        _bitmask |= 1 << 0
      } else {
        if (a.get) {
          _bitmask |= 1 << 1
        }
      }
      if (b == None) {
        _bitmask |= 1 << 2
      } else {
        if (b.get) {
          _bitmask |= 1 << 3
        }
      }
      val _s = if (s == None) {
        null
      } else {
        s.get.getBytes
      }
      (_s, _bitmask)
    }
  }
}
