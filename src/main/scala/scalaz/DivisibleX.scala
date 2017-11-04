// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

/** Abstracts over arity of contravariant parameters on a typeclass. */
sealed abstract class Param[Z, F[_]] {
  type T
  def apply(z: Z): T
  def tc: F[T]
}
object Param {
  def apply[Z, F[_], A](f: Z => A)(FA: => F[A]) = new Param[Z, F] {
    type T = A
    override def apply(z: Z): T = f(z)
    override def tc: F[T]       = FA
  }
}

/** Implementation of Divisible in terms of a single, generic, method. */
trait DivisibleX[F[_]] extends LazyDivisible[F] {
  def divideX[C[_]: Foldable, Z](params: C[Param[Z, F]]): F[Z]

  override def conquer[Z]: F[Z] = divideX[IList, Z] { IList.empty }

  override def contramap[A1, Z](a1: F[A1])(f: Z => A1): F[Z] =
    divideX[NonEmptyList, Z] { NonEmptyList(Param[Z, F, A1](f)(a1)) }
  override def divide2[A1, A2, Z](a1: => F[A1],
                                  a2: => F[A2])(f: Z => (A1, A2)): F[Z] = {
    val mf = Memo.immutableListMapMemo(f) // identityMap would be better
    divideX[NonEmptyList, Z] {
      NonEmptyList(
        Param[Z, F, A1](z => mf(z)._1)(a1),
        Param[Z, F, A2](z => mf(z)._2)(a2)
      )
    }
  }
  override def divide3[A1, A2, A3, Z](a1: => F[A1], a2: => F[A2], a3: => F[A3])(
    f: Z => (A1, A2, A3)
  ): F[Z] = {
    val mf = Memo.immutableListMapMemo(f)
    divideX[NonEmptyList, Z] {
      NonEmptyList(
        Param[Z, F, A1](z => mf(z)._1)(a1),
        Param[Z, F, A2](z => mf(z)._2)(a2),
        Param[Z, F, A3](z => mf(z)._3)(a3)
      )
    }
  }
  override def divide4[A1, A2, A3, A4, Z](a1: => F[A1],
                                          a2: => F[A2],
                                          a3: => F[A3],
                                          a4: => F[A4])(
    f: Z => (A1, A2, A3, A4)
  ): F[Z] = {
    val mf = Memo.immutableListMapMemo(f)
    divideX[NonEmptyList, Z] {
      NonEmptyList(
        Param[Z, F, A1](z => mf(z)._1)(a1),
        Param[Z, F, A2](z => mf(z)._2)(a2),
        Param[Z, F, A3](z => mf(z)._3)(a3),
        Param[Z, F, A4](z => mf(z)._4)(a4)
      )
    }
  }
}
object DivisibleX {
  @inline def apply[F[_]](implicit i: DivisibleX[F]): DivisibleX[F] = i
}
