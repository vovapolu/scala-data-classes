//optimiseHeapOptions optimiseHeapBooleans optimiseHeapStrings
class Foo(a: Option[Boolean], b: Option[Boolean])
//---
{
  final class Foo private (private[this] var _bitmask: Long) {
    import _root_.scala._
    import _root_.scala.Predef._

    override def equals(thatAny: Any): Boolean = thatAny match {
      case that: Foo =>
        (this eq that) || this.a == that.a && this.b == that.b
      case _ =>
        false
    }

    override def hashCode: Int = a.hashCode + 13 * b.hashCode
    override def toString: String = "Foo(" + (a.toString + "," + b.toString) + ")"

    def copy(a: Option[Boolean] = this.a, b: Option[Boolean] = this.b): Foo = Foo(a, b)

    def a: Option[Boolean] = if ((_bitmask & 1 << 0) != 0) {
      None
    } else {
      Some((_bitmask & 1 << 1) != 0)
    }
    def b: Option[Boolean] = if ((_bitmask & 1 << 2) != 0) {
      None
    } else {
      Some((_bitmask & 1 << 3) != 0)
    }
  }

  object Foo {
    import _root_.scala._
    import _root_.scala.Predef._

    def unapply(that: Foo): Option[(Option[Boolean], Option[Boolean])] = Some((that.a, that.b))

    override def toString: String = "Foo"

    def apply(a: Option[Boolean], b: Option[Boolean]): Foo = {
      val packed = pack(a, b)
      val created = new Foo(packed)
      created.synchronized(created)
    }

    private def pack(a: Option[Boolean], b: Option[Boolean]): Long = {
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
      _bitmask
    }
  }
}
