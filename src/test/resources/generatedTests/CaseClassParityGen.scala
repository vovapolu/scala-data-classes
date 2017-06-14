class A(a: Boolean, s: String)
---
{
  class A(val a: Boolean, val s: String) extends Product with Serializable {
    override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object]) || (thatAny match {
      case that: A =>
        that.a == this.a && that.s == this.s
      case _ =>
        false
    })
    override def toString(): String = scala.runtime.ScalaRunTime._toString(this)
    override def hashCode(): Int = scala.runtime.ScalaRunTime._hashCode(this)
    def copy(a: Boolean = this.a, s: String = this.s): A = new A(a, s)
    def canEqual(that: Any): Boolean = that.isInstanceOf[A]
    def productArity: Int = 2
    def productElement(n: Int): Any = n match {
      case 0 =>
        this.a
      case 1 =>
        this.s
      case _ =>
        throw new IndexOutOfBoundsException(n.toString())
    }
    override def productPrefix: String = "A"
    override def productIterator: Iterator[Any] = scala.runtime.ScalaRunTime.typedProductIterator[Any](this)
  }

  object A {
    def apply(a: Boolean, s: String): A = new A(a, s)
    def unapply(that: A): Option[(Boolean, String)] = Some((that.a, that.s))
  }
}