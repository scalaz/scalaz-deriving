// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.inline

import iotaz._
import iotaz.TList.::
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

import Scalaz._
import Prods._

/**
 * Generic extension of Divisible implementing DerivingProducts, with a convenient API.
 */
trait Divisiblez[F[_]] extends InvariantApplicativez[F] with Divisible[F] {
  @unused type G[a] = Id[a]
  @unused def G: Applicative[G] = Applicative[Id]

  /**
   * This is only visible to implementors, it is not part of the public API.
   * Implementors may also choose to implement dividez directly for performance
   * reasons.
   */
  protected def productz[Z, H[_]: Traverse](f: Z =*> H): F[Z]

  // implementation...
  final protected def productz[Z, H[_]: Traverse](
    @unused f: (F ~> G) => G[Z],
    g: Z =*> H
  ): F[Z] = productz(g)

  def dividez[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    g: Z => Prod[L]
  )(
    implicit ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = xproductz(tcs)(null, g) // scalafix:ok
  // covariant param is ignored

  // derived combinators
  override def conquer[Z]: F[Z] =
    dividez[Z, TNil, TNil](empty)(_ => empty)

  override def xmap[A, B](ma: F[A], @unused f: A => B, g: B => A): F[B] =
    contramap(ma)(g)

  override def contramap[A1, Z](a1: F[A1])(f: Z => A1): F[Z] =
    dividez(Prod(Value(a1)))(z => Prod[A1 :: TNil](f(z)))

  override def divide[A1, A2, Z](a1: F[A1], a2: F[A2])(
    f: Z => (A1, A2)
  ): F[Z] =
    dividez(LazyProd(a1, a2))(z => from2T(f(z)))
  override def divide3[A1, A2, A3, Z](a1: F[A1], a2: F[A2], a3: F[A3])(
    f: Z => (A1, A2, A3)
  ): F[Z] =
    dividez(LazyProd(a1, a2, a3))(z => from3T(f(z)))
  override def divide4[A1, A2, A3, A4, Z](
    a1: F[A1],
    a2: F[A2],
    a3: F[A3],
    a4: F[A4]
  )(
    f: Z => (A1, A2, A3, A4)
  ): F[Z] =
    dividez(LazyProd(a1, a2, a3, a4))(z => from4T(f(z)))
  // scalaz goes all the way to divide22, but we give up here for brevity

}
object Divisiblez {
  @inline def apply[F[_]](implicit i: Divisiblez[F]): Divisiblez[F] = i
}
