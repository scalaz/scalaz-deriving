// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.inline
import scala.collection.immutable.Seq

import iotaz._
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

import Scalaz._

/**
 * For decoder algebras (e.g. json, xml) that require access to label
 * information, thus cannot implement a lawful Alt.
 */
abstract class LabelledDecoder[F[_]] extends Deriving[F] {

  type G[_]
  def G: Applicative[G]

  type LFA[a] = (String, F[a])

  protected def productz[Z](f: (LFA ~> G) => G[Z]): F[Z]
  protected def coproductz[Z](
    f: (LFA ~> EphemeralStream) => EphemeralStream[Z]
  ): F[Z]

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
  ): F[Z] = {
    implicit val GA: Applicative[G] = G

    productz { (faa: LFA ~> G) =>
      labels.values
        .zip(tcs.values)
        .asInstanceOf[Seq[(String, Name[F[scala.Any]])]]
        .toList
        .traverse { case (lab, nfa) => faa((lab, nfa.value)) }
        .map(v => f(Prod.unsafeApply(v)))
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
  ): F[Z] =
    coproductz { (faa: LFA ~> EphemeralStream) =>
      labels.values
        .zip(tcs.values)
        .asInstanceOf[Seq[(String, Name[F[scala.Any]])]]
        .toList
        .indexed
        .toEphemeralStream // a FromFoldable would allow abstraction
        .flatMap {
          case (i, (lab, nfa)) =>
            val t: F[scala.Any]                = nfa.value
            val ys: EphemeralStream[scala.Any] = faa((lab, t))
            ys.map(y => (i, y)) // from implied InjectL
        }
        .map { case (i, y) => f(Cop.unsafeApply(i, y)) }
    }

}
object LabelledDecoder {
  @inline def apply[F[_]](implicit F: LabelledDecoder[F]): LabelledDecoder[F] =
    F
}
