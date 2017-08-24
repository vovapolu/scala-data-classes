//memoiseHashCodeLazy memoiseToStringLazy
class A[T](i: Int, b: Boolean, t: T)
//---
{
  final class A[T] private (private[this] var _i: Int, private[this] var _b: Boolean, private[this] var _t: T) {
    import _root_.scala._
    import _root_.scala.Predef._

    def i: Int = this._i
    def b: Boolean = this._b
    def t: T = this._t

    override def equals(thatAny: Any): Boolean = (thatAny match {
      case that: A[_] =>
        (this eq that) || (this.i == that.i && this.b == that.b && this.t == that.t)
      case _ =>
        false
    })

    override lazy val hashCode: Int = i.hashCode + 13 * (b.hashCode + 13 * t.hashCode)
    override lazy val toString: String = "A(" + (i.toString + "," + b.toString + "," + t.toString) + ")"

    def copy[N$T](i: Int = this.i, b: Boolean = this.b, t: N$T = this.t): A[N$T] = A(i, b, t)
  }

  object A {
    import _root_.scala._
    import _root_.scala.Predef._

    def apply[T](i: Int, b: Boolean, t: T): A[T] = {
      val created = new A(i, b, t)
      created.synchronized(created)
    }

    def unapply[T](that: A[T]): Option[(Int, Boolean, T)] = Some((that.i, that.b, that.t))

    override def toString: String = "A"
  }
}