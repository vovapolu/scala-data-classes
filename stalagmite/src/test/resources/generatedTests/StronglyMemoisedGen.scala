//memoise memoiseStrong memoiseHashCode memoiseToString
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

    override val hashCode: Int = i.hashCode + 13 * (t.hashCode + 13 * b.hashCode)
    override val toString: String = "A(" + (i.toString + "," + t.toString + "," + b.toString) + ")"

    def copy[N$T](i: Int = this.i, t: N$T = this.t, b: Boolean = this.b): A[N$T] = A(i, t, b)
  }

  object A {
    import _root_.scala._
    import _root_.scala.Predef._

    def unapply[T](that: A[T]): Option[(Int, T, Boolean)] = Some((that.i, that.t, that.b))

    override def toString: String = "A"

    def apply[T](i: Int, t: T, b: Boolean): A[T] = {
      val t_memoised = memoisedRef_cache.intern(t).asInstanceOf[T]
      val created = new A(i, t_memoised, b)
      val safe = created.synchronized(created)
      memoised_cache.intern(new AWithValueEquality[T](safe)).d
    }

    private class AWithValueEquality[T](val d: A[T]) {
      override def toString: String = d.toString
      override def hashCode: Int = d.hashCode
      override def equals(o: Any): Boolean = o match {
        case that: AWithValueEquality[_] if this.hashCode == that.hashCode =>
          this.d.i == that.d.i && this.d.t == that.d.t && this.d.b == that.d.b
        case _ =>
          false
      }
    }
    private[this] val memoised_cache = _root_.com.google.common.collect.Interners.newStrongInterner[AWithValueEquality[_]]()
    private[this] val memoisedRef_cache = _root_.com.google.common.collect.Interners.newStrongInterner[AnyRef]()
  }
}
