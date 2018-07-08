// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline, Any }

import iotaz._
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

import Cops._
import Scalaz._

/**
 * Generic extension of Decidable implementing Deriving, with a convenient API.
 */
trait Decidablez[F[_]]
    extends Divisiblez[F]
    with InvariantAltz[F]
    with Decidable[F] {

  /**
   * Implementors can implement this or override choosez.
   *
   * This method adds some type safety to the raw iotaz API, at a small
   * performance penalty and incurring extra restrictions.
   */
  protected def coproductz[Z](f: Z =+> Maybe): F[Z]
  type =+>[Z, H[_]] = ArityExists1[Z, F, H]

  /** Implementors may override this, subject to limitations of the iotaz API. */
  def choosez[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    g: Z => Cop[L]
  )(
    implicit @unused ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = {
    import /~\.T2
    val gz = new (Z =+> Maybe) {
      def apply(z: Z): F /~\ Id = {
        val co = g(z)
        val tc = tcs.values(co.index).asInstanceOf[Name[F[Any]]]
        /~\[F, Id, Any](tc.value, co.value)
      }
      def apply(z1: Z, z2: Z): Maybe[F /~\ T2] = {
        val co1 = g(z1)
        val co2 = g(z2)
        if (co1.index != co2.index) Maybe.empty
        else {
          val tc = tcs.values(co1.index).asInstanceOf[Name[F[Any]]]
          Maybe.just(/~\[F, T2, Any](tc.value, (co1.value, co2.value)))
        }
      }
    }

    coproductz(gz)
  }

  // derived combinators
  override final def xcoproductz[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    @unused f: Cop[L] => Z,
    g: Z => Cop[L]
  )(
    implicit ev1: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = choosez(tcs)(g)

  override def choose1[Z, A1](a1: =>F[A1])(f: Z => A1): F[Z] =
    choosez(LazyProd(a1))(z => from1(f(z)))
  override def choose2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: Z => A1 \/ A2
  ): F[Z] =
    choosez(LazyProd(a1, a2))(z => from2(f(z)))
  override def choose3[Z, A1, A2, A3](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3]
  )(f: Z => A1 \/ (A2 \/ A3)): F[Z] =
    choosez(LazyProd(a1, a2, a3))(z => from3(f(z)))
  override def choose4[Z, A1, A2, A3, A4](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3],
    a4: =>F[A4]
  )(f: Z => A1 \/ (A2 \/ (A3 \/ A4))): F[Z] =
    choosez(LazyProd(a1, a2, a3, a4))(z => from4(f(z)))

}
object Decidablez {
  @inline def apply[F[_]](implicit i: Decidablez[F]): Decidablez[F] = i
}
