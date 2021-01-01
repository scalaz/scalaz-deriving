// Copyright: 2017 - 2021 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.inline

import iotaz._
import iotaz.TList.::
import Prods._

/**
 * Generic extension of Applicative implementing DerivingProduct.
 */
trait Applicativez[F[_]] extends Applicative[F] with InvariantApplicativez[F] {

  def applyz[Z, A <: TList, FA <: TList](tcs: Prod[FA])(f: Prod[A] => Z)(
    implicit ev: A PairedWith FA
  ): F[Z]

  // derived combinators
  override final def xproductz[Z, A <: TList, FA <: TList](
    tcs: Prod[FA]
  )(
    f: Prod[A] => Z,
    @unused g: Z => Prod[A]
  )(implicit
    ev: A PairedWith FA
  ): F[Z] = applyz(tcs)(f)

  override def ap[A, B](fa: =>F[A])(f: =>F[A => B]): F[B] =
    apply2(fa, f)((a, abc) => abc(a))

  override def point[Z](z: =>Z): F[Z] =
    applyz[Z, TNil, TNil](empty)(_ => z)

  override def xmap[A, B](ma: F[A], f: A => B, @unused g: B => A): F[B] =
    map(ma)(f)

  override def map[A1, Z](a1: F[A1])(f: A1 => Z): F[Z] = {
    type L = A1 :: TNil
    applyz(Prod(Value(a1)))((a: Prod[L]) => f(to1T(a)))
  }

  override def apply2[A1, A2, Z](a1: =>F[A1], a2: =>F[A2])(
    f: (A1, A2) => Z
  ): F[Z] = {
    type L = A1 :: A2 :: TNil
    applyz(LazyProd(a1, a2))((as: Prod[L]) => f.tupled(to2T(as)))
  }
  override def apply3[A1, A2, A3, Z](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1, A2, A3) => Z
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: TNil
    applyz(LazyProd(a1, a2, a3))((as: Prod[L]) => f.tupled(to3T(as)))
  }
  override def apply4[A1, A2, A3, A4, Z](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3],
    a4: =>F[A4]
  )(
    f: (A1, A2, A3, A4) => Z
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: A4 :: TNil
    applyz(LazyProd(a1, a2, a3, a4))((as: Prod[L]) => f.tupled(to4T(as)))
  }
  // scalaz goes all the way to apply12, but we give up here for brevity

}
object Applicativez {
  @inline def apply[F[_]](implicit i: Applicativez[F]): Applicativez[F] = i
}
