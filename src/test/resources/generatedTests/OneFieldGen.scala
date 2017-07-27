//
class A(one: Int)
//---
{
  final class A private (private[this] var _one: Int) {
    import _root_.scala._
    import _root_.scala.Predef._

    def one: Int = this._one

    override def equals(thatAny: Any): Boolean = thatAny match {
      case that: A =>
        (this eq that) || that.one == this.one
      case _ =>
        false
    }

    override def hashCode: Int       = one.hashCode
    override def toString: String    = "A(" + one.toString + ")"

    def copy(one: Int = this.one): A = A(one)
  }

  object A {
    import _root_.scala._
    import _root_.scala.Predef._

    def apply(one: Int): A = {
      val created = new A(one)
      created.synchronized(created)
    }

    def unapply(that: A): Option[Int] = Some(that.one)

    override def toString: String     = "A"
  }
}
