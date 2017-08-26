//optimiseHeapOptions optimiseHeapBooleans optimiseHeapStrings
class Foo(s: Option[String])
//---
{
  final class Foo private (private[this] var _s: Array[Byte], private[this] var _bitmask: Long) {
    import _root_.scala._
    import _root_.scala.Predef._

    override def equals(thatAny: Any): Boolean = thatAny match {
      case that: Foo =>
        (this eq that) || this.s == that.s
      case _ =>
        false
    }

    override def hashCode: Int = s.hashCode
    override def toString: String = "Foo(" + s.toString + ")"

    def copy(s: Option[String] = this.s): Foo = Foo(s)

    def s: Option[String] = if (this._s == null) {
      None
    } else {
      Some(new String(this._s))
    }
  }

  object Foo {
    import _root_.scala._
    import _root_.scala.Predef._

    def unapply(that: Foo): Option[Option[String]] = Some(that.s)

    override def toString: String = "Foo"

    def apply(s: Option[String]): Foo = {
      val packed = pack(s)
      val created = new Foo(packed._1, packed._2)
      created.synchronized(created)
    }

    private def pack(s: Option[String]): (Array[Byte], Long) = {
      var _bitmask: Long = 0L
      val _s = if (s == None) {
        null
      } else {
        s.get.getBytes
      }
      (_s, _bitmask)
    }
  }
}
