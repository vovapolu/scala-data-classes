//memoiseHashCodeLazy memoiseToStringLazy
class A[T](i: Int, b: Boolean, t: T)
//---
{
  final class A[T] private (private[this] val _i: Int, private[this] val _b: Boolean, private[this] val _t: T) {
    def i: Int = this._i
    def b: Boolean = this._b
    def t: T = this._t

    override def equals(thatAny: _root_.scala.Any): _root_.scala.Boolean = thatAny match {
      case that: A[_] =>
        (this eq that) || this.i == that.i && this.b == that.b && this.t == that.t
      case _ =>
        false
    }

    @_root_.scala.transient override lazy val hashCode: _root_.scala.Int = i.hashCode + 13 * (b.hashCode + 13 * t.hashCode)
    @_root_.scala.transient override lazy val toString: _root_.java.lang.String = "A(" + (i.toString + "," + b.toString + "," + t.toString) + ")"

    def copy[N$T](i: Int = this.i, b: Boolean = this.b, t: N$T = this.t): A[N$T] = A(i, b, t)
  }

  object A {
    def apply[T](i: Int, b: Boolean, t: T): A[T] = {
      val created = new A(i, b, t)
      created
    }

    def unapply[T](that: A[T]): _root_.scala.Option[(Int, Boolean, T)] = _root_.scala.Some((that.i, that.b, that.t))

    override def toString: _root_.java.lang.String = "A"
  }
}
