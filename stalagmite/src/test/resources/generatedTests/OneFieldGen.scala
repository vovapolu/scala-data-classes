// shapeless
class A(one: Int)
//---
{
  final class A private (private[this] val _one: Int) {
    import _root_.scala._
    import _root_.scala.Predef._
    def one: Int = this._one

    override def equals(thatAny: Any): Boolean = thatAny match {
      case that: A =>
        (this eq that) || this.one == that.one
      case _ =>
        false
    }

    override def hashCode: Int = one.hashCode
    override def toString: String = "A(" + one.toString + ")"

    def copy(one: Int = this.one): A = A(one)
  }

  object A {
    import _root_.scala._
    import _root_.scala.Predef._

    def apply(one: Int): A = {
      val created = new A(one)
      created
    }

    def unapply(that: A): Option[Int] = Some(that.one)
    override def toString: String = "A"

    import _root_.shapeless.{ ::, HNil, Generic, LabelledGeneric, Typeable }
    import _root_.shapeless.labelled.{ FieldType, field }
    import _root_.shapeless.syntax.singleton._

    val one_tpe = Symbol("one").narrow

    implicit val TypeableA: Typeable[A] = new Typeable[A] {
      override def cast(t: Any): Option[A] = {
        t match {
          case f @ A(one) =>
            Some(A(one))
          case _ =>
            None
        }
      }
      override def describe: String = "A[" + "Int" + "]"
    }

    implicit val GenericA: Generic.Aux[A, Int :: HNil] = new Generic[A] {
      override type Repr = Int :: HNil
      override def to(f: A): Repr = LabelledGenericA.to(f)
      override def from(r: Repr): A = r match {
        case one :: HNil =>
          A(one)
      }
    }

    implicit val LabelledGenericA: LabelledGeneric.Aux[A, FieldType[one_tpe.type, Int] :: HNil] = new LabelledGeneric[A] {
      override type Repr = FieldType[one_tpe.type, Int] :: HNil
      override def to(f: A): Repr = field[one_tpe.type](f.one) :: HNil
      override def from(r: Repr): A = GenericA.from(r)
    }
  }
}
