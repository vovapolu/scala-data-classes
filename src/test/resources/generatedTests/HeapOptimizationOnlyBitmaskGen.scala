//optimiseHeapOptions optimiseHeapBooleans optimiseHeapStrings
class Foo(a: Option[Boolean], b: Option[Boolean])
//---
{
  final class Foo private (private[this] val _bitmask: _root_.scala.Long) {

    override def equals(thatAny: _root_.scala.Any): _root_.scala.Boolean = thatAny match {
      case that: Foo =>
        (this eq that) || this.a == that.a && this.b == that.b
      case _ =>
        false
    }

    override def hashCode: _root_.scala.Int = a.hashCode + 13 * b.hashCode
    override def toString: _root_.java.lang.String = "Foo(" + (a.toString + "," + b.toString) + ")"

    def copy(a: Option[Boolean] = this.a, b: Option[Boolean] = this.b): Foo = Foo(a, b)

    def a: Option[Boolean] = if ((_bitmask & 1 << 0) != 0) {
      _root_.scala.None
    } else {
      _root_.scala.Some((_bitmask & 1 << 1) != 0)
    }

    def b: Option[Boolean] = if ((_bitmask & 1 << 2) != 0) {
      _root_.scala.None
    } else {
      _root_.scala.Some((_bitmask & 1 << 3) != 0)
    }
  }

  object Foo {
    def unapply(that: Foo): _root_.scala.Option[(Option[Boolean], Option[Boolean])] = _root_.scala.Some((that.a, that.b))

    override def toString: _root_.java.lang.String = "Foo"

    def apply(a: Option[Boolean], b: Option[Boolean]): Foo = {
      val packed = pack(a, b)
      val created = new Foo(packed)
      created
    }

    private def pack(a: Option[Boolean], b: Option[Boolean]): _root_.scala.Long = {
      var _bitmask: _root_.scala.Long = 0L
      if (a == _root_.scala.None) {
        _bitmask |= 1 << 0
      } else {
        if (a.get) {
          _bitmask |= 1 << 1
        }
      }
      if (b == _root_.scala.None) {
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
