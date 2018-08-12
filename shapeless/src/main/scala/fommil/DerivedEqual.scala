// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package fommil

import scalaz.Equal
import shapeless._

// lots of copypasta with DerivedOrder

/**
 * Some performance benchmarks show this to be faster than Decidablez[Equal].
 */
sealed trait DerivedEqual[A] extends Equal[A]
object DerivedEqual {
  def gen[A, R](
    implicit G: Generic.Aux[A, R],
    R: Cached[Strict[DerivedEqual[R]]]
  ): Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A): Boolean =
      quick(a1, a2) || R.value.value.equal(G.to(a1), G.to(a2))
  }

  implicit def hcons[H, T <: HList](
    implicit H: Lazy[Equal[H]],
    T: DerivedEqual[T]
  ): DerivedEqual[H :: T] = new DerivedEqual[H :: T] {
    def equal(ht1: H :: T, ht2: H :: T): Boolean =
      (quick(ht1.head, ht2.head) || H.value.equal(ht1.head, ht2.head)) &&
        T.equal(ht1.tail, ht2.tail)
  }

  implicit val hnil: DerivedEqual[HNil] = new DerivedEqual[HNil] {
    def equal(h1: HNil, h2: HNil): Boolean = true
  }

  implicit def ccons[H, T <: Coproduct](
    implicit H: Lazy[Equal[H]],
    T: DerivedEqual[T]
  ): DerivedEqual[H :+: T] = new DerivedEqual[H :+: T] {
    def equal(ht1: H :+: T, ht2: H :+: T): Boolean = (ht1, ht2) match {
      case (Inl(c1), Inl(c2)) => quick(c1, c2) || H.value.equal(c1, c2)
      case (Inr(c1), Inr(c2)) => T.equal(c1, c2)
      case _                  => false
    }
  }

  implicit val cnil: DerivedEqual[CNil] = new DerivedEqual[CNil] {
    def equal(c1: CNil, c2: CNil): Boolean = sys.error("impossible")
  }

  @inline private final def quick(a: Any, b: Any): Boolean =
    a.asInstanceOf[AnyRef].eq(b.asInstanceOf[AnyRef])
}
