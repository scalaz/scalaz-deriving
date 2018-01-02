// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

import iotaz.{ Prod, TList, TNil }
import iotaz.TList.::
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

/** Implementation of Divisible in terms of a single, generic, method. */
trait Divisiblez[F[_]] extends LazyDivisible[F] {
  import Prods._

  def dividez[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Z => Prod[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z]

  override def conquer[Z]: F[Z] =
    dividez[Z, TNil, TNil](empty)(_ => empty)

  override def contramap[A1, Z](a1: F[A1])(f: Z => A1): F[Z] =
    dividez(Prod(Value(a1)))(z => Prod[A1 :: TNil](f(z)))

  override def divide2[A1, A2, Z](a1: =>F[A1],
                                  a2: =>F[A2])(f: Z => (A1, A2)): F[Z] =
    dividez(LazyProd(a1, a2))(z => from2T(f(z)))
  override def divide3[A1, A2, A3, Z](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: Z => (A1, A2, A3)
  ): F[Z] =
    dividez(LazyProd(a1, a2, a3))(z => from3T(f(z)))
  override def divide4[A1, A2, A3, A4, Z](a1: =>F[A1],
                                          a2: =>F[A2],
                                          a3: =>F[A3],
                                          a4: =>F[A4])(
    f: Z => (A1, A2, A3, A4)
  ): F[Z] =
    dividez(LazyProd(a1, a2, a3, a4))(z => from4T(f(z)))
  // scalaz goes all the way to divide22, but we give up here for brevity

}
object Divisiblez {
  @inline def apply[F[_]](implicit i: Divisiblez[F]): Divisiblez[F] = i
}
