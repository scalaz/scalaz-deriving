// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.{ inline, Any }

import iotaz._
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

import Scalaz._

/**
 * Implementations of Derivez that are law-abiding and must therefore ignore
 * labels.
 */
trait LawfulDerivez[F[_]] extends Derivez[F] with Derives[F]

abstract class ContravariantDerivez[F[_]]
    extends LawfulDerivez[F]
    with Codividez[F]
    with Divisiblez[F] {

  // although we don't leave the G as a free parameter, it is a useful reminder
  // to the author of the instance of what they receive after providing a Z.
  type =*>[Z, G[_]] = ArityExists[Z, F, G]
  type =+>[Z, G[_]] = ArityExists1[Z, F, G]

  def productz[Z, G[_]: Traverse](f: Z =*> G): F[Z]
  def coproductz[Z](f: Z =+> Maybe): F[Z]

  final override def xproductz[Z, L <: TList, FL <: TList, N <: TList](
    tcs: Prod[FL],
    labels: Prod[N]
  )(
    f: Prod[L] => Z,
    g: Z => Prod[L]
  )(
    implicit
    ev1: λ[a => Name[F[a]]] ƒ L ↦ FL,
    ev2: λ[a => String] ƒ L ↦ N
  ): F[Z] = dividez(tcs)(g)

  final override def dividez[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    g: Z => Prod[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
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

  final override def xcoproductz[Z, L <: TList, FL <: TList, N <: TList](
    tcs: Prod[FL],
    labels: Prod[N]
  )(
    f: Cop[L] => Z,
    g: Z => Cop[L]
  )(
    implicit
    ev1: λ[a => Name[F[a]]] ƒ L ↦ FL,
    ev2: λ[a => String] ƒ L ↦ N
  ): F[Z] = codividez(tcs)(g)

  final override def codividez[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    g: Z => Cop[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
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

}
object ContravariantDerivez {
  @inline def apply[F[_]](
    implicit i: ContravariantDerivez[F]
  ): ContravariantDerivez[F] = i
}

abstract class CovariantDerivez[F[_], G[_]: Monad: FromFoldable1]
    extends LawfulDerivez[F]
    with Coapplicativez[F]
    with Applicativez[F] {

  def productz[Z](f: (F ~> Id) => Z): F[Z]
  def coproductz[Z](f: (F ~> G) => G[Z]): F[Z]

  final override def xproductz[Z, L <: TList, FL <: TList, N <: TList](
    tcs: Prod[FL],
    labels: Prod[N]
  )(
    f: Prod[L] => Z,
    g: Z => Prod[L]
  )(
    implicit
    ev1: λ[a => Name[F[a]]] ƒ L ↦ FL,
    ev2: λ[a => String] ƒ L ↦ N
  ): F[Z] = applyz(tcs)(f)

  final def applyz[Z, L <: TList, FL <: TList](tcs: Prod[FL])(
    f: Prod[L] => Z
  )(
    implicit ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] =
    productz(((faa: (F ~> Id)) => f(Prods.map(tcs)(faa))))

  final override def xcoproductz[Z, L <: TList, FL <: TList, N <: TList](
    tcs: Prod[FL],
    labels: Prod[N]
  )(
    f: Cop[L] => Z,
    g: Z => Cop[L]
  )(
    implicit
    ev1: λ[a => Name[F[a]]] ƒ L ↦ FL,
    ev2: λ[a => String] ƒ L ↦ N
  ): F[Z] = coapplyz(tcs)(f)

  final override def coapplyz[Z, L <: TList, FL <: TList](tcs: Prod[FL])(
    f: Cop[L] => Z
  )(
    implicit ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] =
    coproductz((faa: (F ~> G)) => Cops.mapMaybe(tcs)(faa).map(f))

}
object CovariantDerivez {
  @inline def apply[F[_], G[_]: Monad: FromFoldable1](
    implicit i: CovariantDerivez[F, G]
  ): CovariantDerivez[F, G] = i
}
