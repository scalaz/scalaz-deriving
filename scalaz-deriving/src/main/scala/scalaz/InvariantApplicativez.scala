// Copyright: 2017 - 2021 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.inline

import iotaz._
import iotaz.TList.::
import Prods._

/**
 * Generic extension of InvariantApplicative implementing DerivingProducts.
 */
trait InvariantApplicativez[F[_]]
    extends InvariantApplicative[F]
    with DerivingProducts[F] {

  override def xmap[A, B](ma: F[A], f: A => B, g: B => A): F[B] =
    xproduct1(ma)(f, g)

  override def xproduct0[Z](z: =>Z): F[Z] =
    xproductz[Z, TNil, TNil](empty)(_ => z, _ => empty)
  override def xproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z] = {
    type L = A1 :: TNil
    xproductz(LazyProd(a1))(
      (a: Prod[L]) => f(to1T(a)),
      z => Prod[A1 :: TNil](g(z))
    )
  }
  override def xproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: (A1, A2) => Z,
    g: Z => (A1, A2)
  ): F[Z] = {
    type L = A1 :: A2 :: TNil
    xproductz(LazyProd(a1, a2))(
      (as: Prod[L]) => f.tupled(to2T(as)),
      z => from2T(g(z))
    )
  }
  override def xproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: TNil
    xproductz(LazyProd(a1, a2, a3))(
      (as: Prod[L]) => f.tupled(to3T(as)),
      z => from3T(g(z))
    )
  }
  override def xproduct4[Z, A1, A2, A3, A4](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3],
    a4: =>F[A4]
  )(
    f: (A1, A2, A3, A4) => Z,
    g: Z => (A1, A2, A3, A4)
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: A4 :: TNil
    xproductz(LazyProd(a1, a2, a3, a4))(
      (as: Prod[L]) => f.tupled(to4T(as)),
      z => from4T(g(z))
    )
  }

}
object InvariantApplicativez {
  @inline def apply[F[_]](implicit
    i: InvariantApplicativez[F]
  ): InvariantApplicativez[F] = i
}
