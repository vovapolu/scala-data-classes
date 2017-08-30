//optimiseHeapOptions optimiseHeapBooleans optimiseHeapStrings
class Foo(s: Option[String])
//---
{
  final class Foo private (private[this] val _s: _root_.scala.Array[_root_.scala.Byte], private[this] val _bitmask: _root_.scala.Long) {
    override def equals(thatAny: _root_.scala.Any): _root_.scala.Boolean = thatAny match {
      case that: Foo =>
        (this eq that) || this.s == that.s
      case _ =>
        false
    }

    override def hashCode: _root_.scala.Int = s.hashCode
    override def toString: _root_.java.lang.String = "Foo(" + s.toString + ")"

    def copy(s: Option[String] = this.s): Foo = Foo(s)

    def s: Option[String] = if (this._s == null) {
      _root_.scala.None
    } else {
      _root_.scala.Some(new String(this._s))
    }
  }

  object Foo {
    def unapply(that: Foo): _root_.scala.Option[Option[String]] = _root_.scala.Some(that.s)

    override def toString: _root_.java.lang.String = "Foo"

    def apply(s: Option[String]): Foo = {
      val packed = pack(s)
      val created = new Foo(packed._1, packed._2)
      created
    }

    private def pack(s: Option[String]): (_root_.scala.Array[_root_.scala.Byte], _root_.scala.Long) = {
      var _bitmask: _root_.scala.Long = 0L
      val _s = if (s == _root_.scala.None) {
        null
      } else {
        s.get.getBytes
      }
      (_s, _bitmask)
    }
  }
}

