// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline, Any }
import scala.collection.immutable.List

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

  /**
   * Implementors can implement this or override dividez.
   *
   * This method adds some type safety to the raw iotaz API, at a small
   * performance penalty and incurring extra restrictions.
   */
  protected def productz[Z, H[_]: Traverse](f: Z =*> H): F[Z]
  type =*>[Z, H[_]] = ArityExists[Z, F, H]

  /** Implementors may override this, subject to limitations of the iotaz API. */
  def dividez[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    g: Z => Prod[L]
  )(
    implicit @unused ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = {
    import /~\.T2
    val gz = new (Z =*> List) {
      override def apply(z: Z): List[F /~\ Id] =
        g(z).values
          .zip(tcs.values)
          .map { tcv =>
            /~\[F, Id, Any](tcv._2.asInstanceOf[Name[F[Any]]].value, tcv._1)
          }(scala.collection.breakOut)
      override def apply(z1: Z, z2: Z): List[F /~\ T2] =
        g(z1).values
          .zip(g(z2).values)
          .zip(tcs.values)
          .map { tcv =>
            val ((v1, v2), tc) = tcv
            /~\[F, T2, Any](tc.asInstanceOf[Name[F[Any]]].value, (v1, v2))
          }(scala.collection.breakOut)
    }
    productz(gz)
  }

  // derived combinators
  override final def xproductz[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    @unused f: Prod[L] => Z,
    g: Z => Prod[L]
  )(
    implicit ev1: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = dividez(tcs)(g)

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
