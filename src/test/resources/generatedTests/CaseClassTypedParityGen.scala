class A[T](a: Boolean, s: String, t: T)
---
{
  final class A[T] private (private[this] var _a: Boolean, private[this] var _s: String, private[this] var _t: T) extends Product with Serializable {
    def a: Boolean = this._a
    def s: String = this._s
    def t: T = this._t

    override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object]) || (thatAny match {
      case that: A[_] =>
        that.a == this.a && that.s == this.s && that.t == this.t
      case _ =>
        false
    })

    override def hashCode(): Int = a.hashCode + 13 * (s.hashCode + 13 * t.hashCode)
    override def toString(): String = "A" + "(" + a.toString + s.toString + t.toString + ")"

    @throws[java.io.IOException] private[this] def writeObject(out: java.io.ObjectOutputStream): Unit = {
      out.writeBoolean(a)
      out.writeUTF(s)
      out.writeObject(t)
    }

    @throws[java.io.IOException]
    @throws[ClassNotFoundException]
    private[this] def readObject(in: java.io.ObjectInputStream): Unit = {
      _a = in.readBoolean()
      _s = in.readUTF()
      _t = in.readObject().asInstanceOf[T]
    }

    @throws[java.io.ObjectStreamException]
    private[this] def readResolve(): Any = A(a, s, t)


    def copy(a: Boolean = this.a, s: String = this.s, t: T = this.t): A[T] = new A(a, s, t)
    def canEqual(that: Any): Boolean = that.isInstanceOf[A[T]]
    def productArity: Int = 3
    def productElement(n: Int): Any = n match {
      case 0 =>
        this.a
      case 1 =>
        this.s
      case 2 =>
        this.t
      case _ =>
        throw new IndexOutOfBoundsException(n.toString())
    }
    override def productPrefix: String = "A"
    override def productIterator: Iterator[Any] = scala.runtime.ScalaRunTime.typedProductIterator[Any](this)
  }

  object A {
    def apply[T](a: Boolean, s: String, t: T): A[T] = new A(a, s, t)
    def unapply[T](that: A[T]): Option[(Boolean, String, T)] = Some((that.a, that.s, that.t))
  }
}