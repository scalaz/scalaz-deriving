// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.inline

import iotaz._
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

import Scalaz._

/**
 * For decoder algebras (e.g. json, xml) that require access to label
 * information, thus cannot implement a lawful Alt.
 */
abstract class LabelledDecoder[F[_]] extends Deriving[F] with Functor[F] {

  def productz[Z](f: (F ~> Id) => Z): F[Z]
  def coproductz[Z](f: (F ~> Maybe) => EphemeralStream[Z]): F[Z]

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
  ): F[Z] = productz(((faa: (F ~> Id)) => f(Prods.map(tcs)(faa))))

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
  ): F[Z] = coproductz((faa: (F ~> Maybe)) => Cops.mapMaybe(tcs)(faa).map(f))

}
object LabelledDecoder {
  @inline def apply[F[_]](implicit F: LabelledDecoder[F]): LabelledDecoder[F] =
    F
}
