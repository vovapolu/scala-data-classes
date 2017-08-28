//memoise memoiseEqualsByValue
//memoiseRefs t
class A[T](i: Int, t: T, b: Boolean)
//---
{
  final class A[T] private (private[this] val _i: Int, private[this] val _t: T, private[this] val _b: Boolean,
                            @transient private val _key: (Int, AnyRef, Boolean)) {
    import _root_.scala._
    import _root_.scala.Predef._

    def i: Int = this._i
    def t: T = this._t
    def b: Boolean = this._b

    override def equals(thatAny: Any): Boolean = (thatAny match {
      case that: A[_] =>
        (this eq that) || (this.i == that.i && this.t == that.t && this.b == that.b)
      case _ =>
        false
    })

    override def hashCode: Int = i.hashCode + 13 * (t.hashCode + 13 * b.hashCode)
    override def toString: String = "A(" + (i.toString + "," + t.toString + "," + b.toString) + ")"

    def copy[N$T](i: Int = this.i, t: N$T = this.t, b: Boolean = this.b): A[N$T] = A(i, t, b)
  }

  object A {
    import _root_.scala._
    import _root_.scala.Predef._

    def unapply[T](that: A[T]): Option[(Int, T, Boolean)] = Some((that.i, that.t, that.b))

    override def toString: String = "A"

    def apply[T](i: Int, t: T, b: Boolean): A[T] = {
      val t_memoised = memoisedRef(t).asInstanceOf[T]
      val key: (Int, AnyRef, Boolean) = (i, t_memoised, b)
      val first = {
        val weak = memoised_cache.get(key)
        if (weak == null) null else weak.get
      }
      if (first != null) {
        first
      } else {
        memoised_cache.synchronized {
          val got = {
            val weak = memoised_cache.get(key)
            if (weak == null) {
              null
            } else {
              val ref = weak.get
              ref
            }
          }
          if (got != null) {
            got
          } else {
            val created = new A(i, t_memoised, b, key)
            memoised_cache.put(key, new _root_.java.lang.ref.WeakReference(created))
            created
          }
        }
      }
    }

    private[this] val memoised_cache = new _root_.java.util.WeakHashMap[(Int, AnyRef, Boolean), _root_.java.lang.ref.WeakReference[A[_]]]
    private[this] val memoisedRef_cache = new _root_.java.util.WeakHashMap[AnyRef, _root_.java.lang.ref.WeakReference[AnyRef]]

    private[this] def memoisedRef(ref: AnyRef): AnyRef = {
      val first = {
        val weak = memoisedRef_cache.get(ref)
        if (weak == null) null else weak.get
      }
      if (first != null) first else memoisedRef_cache.synchronized {
        val got = {
          val weak = memoisedRef_cache.get(ref)
          if (weak == null) null else weak.get
        }
        if (got != null) got else {
          memoisedRef_cache.put(ref, new _root_.java.lang.ref.WeakReference(ref))
          ref
        }
      }
    }
  }
}
