// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.{ inline, Any }

import iotaz._
import iotaz.TList.::
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

import Scalaz._
import Prods._
import Cops._

/**
 * Generic extension of Decidable implementing Deriving, with a convenient API.
 */
trait Decidablez[F[_]] extends Decidable[F] with Deriving[F] {

  type =*>[Z, G[_]] = ArityExists[Z, F, G]
  type =+>[Z, G[_]] = ArityExists1[Z, F, G]

  /**
   * This is only visible to implementors, it is not part of the public API.
   * Implementors may also choose to implement dividez directly for performance
   * reasons.
   */
  protected def productz[Z, G[_]: Traverse](f: Z =*> G): F[Z]

  /**
   * This is only visible to implementors, it is not part of the public API.
   * Implementors may also choose to implement choosez directly for performance
   * reasons.
   */
  protected def coproductz[Z](f: Z =+> Maybe): F[Z]

  override final def xproductz[Z, L <: TList, FL <: TList, N <: TList](
    tcs: Prod[FL],
    @unused labels: Prod[N]
  )(
    f: Prod[L] => Z,
    g: Z => Prod[L]
  )(
    implicit
    ev1: λ[a => Name[F[a]]] ƒ L ↦ FL,
    ev2: λ[a => String] ƒ L ↦ N
  ): F[Z] = dividez(tcs)(g)

  def dividez[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    g: Z => Prod[L]
  )(
    implicit
    @unused ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = productz {
    import /~\.T2
    import scala.collection.immutable.List
    new (Z =*> List) {
      def apply(z: Z): List[F /~\ Id] =
        g(z).values
          .zip(tcs.values)
          .map { tcv =>
            /~\[F, Id, Any](tcv._2.asInstanceOf[Name[F[Any]]].value, tcv._1)
          }(scala.collection.breakOut)
      def apply(z1: Z, z2: Z): List[F /~\ T2] =
        g(z1).values
          .zip(g(z2).values)
          .zip(tcs.values)
          .map { tcv =>
            val ((v1, v2), tc) = tcv
            /~\[F, T2, Any](tc.asInstanceOf[Name[F[Any]]].value, (v1, v2))
          }(scala.collection.breakOut)
    }
  }

  override final def xcoproductz[Z, L <: TList, FL <: TList, N <: TList](
    tcs: Prod[FL],
    @unused labels: Prod[N]
  )(
    f: Cop[L] => Z,
    g: Z => Cop[L]
  )(
    implicit
    ev1: λ[a => Name[F[a]]] ƒ L ↦ FL,
    ev2: λ[a => String] ƒ L ↦ N
  ): F[Z] = choosez(tcs)(g)

  def choosez[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    g: Z => Cop[L]
  )(
    implicit
    @unused ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = coproductz {
    import /~\.T2
    new (Z =+> Maybe) {
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
  }

  override def conquer[Z]: F[Z] =
    dividez[Z, TNil, TNil](empty)(_ => empty)

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
