//memoise memoiseStrong memoiseHashCode memoiseToString
//memoiseRefs t
class A[T](i: Int, t: T, b: Boolean)
//---
{
  final class A[T] private (private[this] val _i: Int, private[this] val _t: T, private[this] val _b: Boolean) {

    def i: Int = this._i
    def t: T = this._t
    def b: Boolean = this._b

    @_root_.scala.transient override val hashCode: _root_.scala.Int = i.hashCode + 13 * (t.hashCode + 13 * b.hashCode)
    @_root_.scala.transient override val toString: _root_.java.lang.String = "A(" + (i.toString + "," + t.toString + "," + b.toString) + ")"

    def copy[N$T](i: Int = this.i, t: N$T = this.t, b: Boolean = this.b): A[N$T] = A(i, t, b)
  }

  object A {
    def unapply[T](that: A[T]): _root_.scala.Option[(Int, T, Boolean)] = _root_.scala.Some((that.i, that.t, that.b))

    override def toString: _root_.java.lang.String = "A"

    def apply[T](i: Int, t: T, b: Boolean): A[T] = {
      val t_memoised = memoisedRef_cache.intern(t).asInstanceOf[T]
      val created = new A(i, t_memoised, b)
      memoised_cache.intern(new AWithValueEquality[T](created)).d
    }

    private class AWithValueEquality[T](val d: A[T]) {
      override def toString: _root_.java.lang.String = d.toString
      override def hashCode: _root_.scala.Int = d.hashCode
      override def equals(o: _root_.scala.Any): _root_.scala.Boolean = o match {
        case that: AWithValueEquality[_] if this.hashCode == that.hashCode =>
          this.d.i == that.d.i && this.d.t == that.d.t && this.d.b == that.d.b
        case _ =>
          false
      }
    }

    private[this] val memoised_cache = _root_.com.google.common.collect.Interners.newStrongInterner[AWithValueEquality[_]]()
    private[this] val memoisedRef_cache = _root_.com.google.common.collect.Interners.newStrongInterner[_root_.scala.AnyRef]()
  }
}
