// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package fommil

import scalaz.{ Order, Ordering }
import shapeless._

// lots of copypasta with DerivedEqual
sealed trait DerivedOrder[A] extends Order[A]
object DerivedOrder {
  def gen[A, R](implicit
    G: Generic.Aux[A, R],
    R: Cached[Strict[DerivedOrder[R]]]
  ): Order[A] =
    new Order[A] {
      override def equal(a1: A, a2: A): Boolean =
        quick(a1, a2) || R.value.value.equal(G.to(a1), G.to(a2))

      def order(a1: A, a2: A): Ordering =
        if (quick(a1, a2)) Ordering.EQ
        else R.value.value.order(G.to(a1), G.to(a2))
    }

  implicit def hcons[H, T <: HList](implicit
    H: Lazy[Order[H]],
    T: DerivedOrder[T]
  ): DerivedOrder[H :: T] =
    new DerivedOrder[H :: T] {
      override def equal(ht1: H :: T, ht2: H :: T): Boolean =
        (quick(ht1.head, ht2.head) || H.value.equal(ht1.head, ht2.head)) &&
          T.equal(ht1.tail, ht2.tail)

      def order(ht1: H :: T, ht2: H :: T): Ordering =
        if (quick(ht1.head, ht2.head)) T.order(ht1.tail, ht2.tail)
        else
          H.value.order(ht1.head, ht2.head) match {
            case Ordering.EQ => T.order(ht1.tail, ht2.tail)
            case neq         => neq
          }
    }

  implicit val hnil: DerivedOrder[HNil] = new DerivedOrder[HNil] {
    override def equal(h1: HNil, h2: HNil): Boolean = true
    def order(h1: HNil, h2: HNil): Ordering         = Ordering.EQ
  }

  implicit def ccons[H, T <: Coproduct](implicit
    H: Lazy[Order[H]],
    T: DerivedOrder[T]
  ): DerivedOrder[H :+: T] =
    new DerivedOrder[H :+: T] {
      override def equal(ht1: H :+: T, ht2: H :+: T): Boolean =
        (ht1, ht2) match {
          case (Inl(c1), Inl(c2)) => quick(c1, c2) || H.value.equal(c1, c2)
          case (Inr(c1), Inr(c2)) => T.equal(c1, c2)
          case _                  => false
        }
      def order(ht1: H :+: T, ht2: H :+: T): Ordering         =
        (ht1, ht2) match {
          case (Inl(c1), Inl(c2)) =>
            if (quick(c1, c2)) Ordering.EQ
            else H.value.order(c1, c2)
          case (Inr(c1), Inr(c2)) => T.order(c1, c2)
          case (Inl(_), _)        => Ordering.LT
          case _                  => Ordering.GT
        }
    }

  implicit val cnil: DerivedOrder[CNil] = new DerivedOrder[CNil] {
    override def equal(c1: CNil, c2: CNil): Boolean = sys.error("impossible")
    def order(c1: CNil, c2: CNil): Ordering         = sys.error("impossible")
  }

  @inline private final def quick(a: Any, b: Any): Boolean =
    a.asInstanceOf[AnyRef].eq(b.asInstanceOf[AnyRef])
}
