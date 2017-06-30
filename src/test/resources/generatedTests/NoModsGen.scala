//
class A[T](i: Int, b: Boolean, t: T)
//---
{
  final class A[T] private (private[this] var _i: Int, private[this] var _b: Boolean, private[this] var _t: T) {
    def i: Int = this._i
    def b: Boolean = this._b
    def t: T = this._t

    override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object]) || (thatAny match {
      case that: A[_] =>
        that.i == this.i && that.b == this.b && that.t == this.t
      case _ =>
        false
    })

    override def hashCode(): Int = i.hashCode + 13 * (b.hashCode + 13 * t.hashCode)
    override def toString: String = "A(" + (i.toString + "," + b.toString + "," + t.toString) + ")"

    def copy(i: Int = this.i, b: Boolean = this.b, t: T = this.t): A[T] = new A(i, b, t)
  }

  object A {
    def apply[T](i: Int, b: Boolean, t: T): A[T] = {
      val newVal = new A(i, b, t)
      newVal.synchronized(newVal)
    }

    def unapply[T](that: A[T]): Option[(Int, Boolean, T)] = Some((that.i, that.b, that.t))

    override def toString: String = "A"
  }
}