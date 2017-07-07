//memoise
//memoiseRefs t
class A[T](i: Int, t: T, b: Boolean)
//---
{
  final class A[T] private (private[this] var _i: Int, private[this] var _t: T, private[this] var _b: Boolean) {
    import _root_.scala._
    import _root_.scala.Predef._

    def i: Int = this._i
    def t: T = this._t
    def b: Boolean = this._b

    override def equals(thatAny: Any): Boolean = thatAny match {
      case that: A[_] =>
        (this eq that) || that.i == this.i && that.t == this.t && that.b == this.b
      case _ =>
        false
    }

    override def hashCode(): Int = i.hashCode + 13 * (t.hashCode + 13 * b.hashCode)
    override def toString: String = "A(" + (i.toString + "," + t.toString + "," + b.toString) + ")"
    def copy[N$T >: T](i: Int = this.i, t: N$T = this.t, b: Boolean = this.b): A[N$T] = A(i, t, b)

    def intern: A[T] = A(i, t, b)
  }

  object A {
    import _root_.scala._
    import _root_.scala.Predef._

    def apply[T](i: Int, t: T, b: Boolean): A[T] = {
      val t_memoised = memoisedRef_cache.intern(t).asInstanceOf[T]
      val created = new A(i, t_memoised, b)
      val safe = created.synchronized(created)
      memoised_cache.intern(safe)
    }

    def unapply[T](that: A[T]): Option[(Int, T, Boolean)] = Some((that.i, that.t, that.b))

    override def toString: String = "A"

    private[this] val memoised_cache = _root_.com.google.common.collect.Interners.newWeakInterner[A[_]]()
    private[this] val memoisedRef_cache = _root_.com.google.common.collect.Interners.newWeakInterner[AnyRef]()
  }
}
