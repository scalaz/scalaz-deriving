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
 * For encoder algebras (e.g. json, xml) that require access to label
 * information, thus cannot implement a lawful Decidable.
 */
abstract class LabelledEncoder[F[_]] extends Deriving[F] {

  type =*>[Z, G[_]] = LabelledArityExists[Z, F, G]
  type =+>[Z, G[_]] = LabelledArityExists1[Z, F, G]

  protected def productz[Z, G[_]: Traverse](f: Z =*> G): F[Z]
  protected def coproductz[Z](f: Z =+> Maybe): F[Z]

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
  ): F[Z] = productz {
    import /~\.{ L1, L2 }
    import scala.collection.immutable.List
    new (Z =*> List) {
      def apply(z: Z): List[F /~\ L1] =
        g(z).values
          .zip(tcs.values)
          .zip(labels.values)
          .map { tcv =>
            val ((v1, v2), lab) = tcv
            /~\[F, L1, Any](
              v2.asInstanceOf[Name[F[Any]]].value,
              (lab.asInstanceOf[String], v1)
            )
          }(scala.collection.breakOut)
      def apply(z1: Z, z2: Z): List[F /~\ L2] =
        g(z1).values
          .zip(g(z2).values)
          .zip(tcs.values)
          .zip(labels.values)
          .map { tcv =>
            val (((v1, v2), tc), lab) = tcv
            /~\[F, L2, Any](
              tc.asInstanceOf[Name[F[Any]]].value,
              (lab.asInstanceOf[String], v1, v2)
            )
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
  ): F[Z] = coproductz {
    import /~\.{ L1, L2 }
    new (Z =+> Maybe) {
      def apply(z: Z): F /~\ L1 = {
        val co  = g(z)
        val tc  = tcs.values(co.index).asInstanceOf[Name[F[Any]]]
        val lab = labels.values(co.index).asInstanceOf[String]
        /~\[F, L1, Any](tc.value, (lab, co.value))
      }
      def apply(z1: Z, z2: Z): Maybe[F /~\ L2] = {
        val co1 = g(z1)
        val co2 = g(z2)
        if (co1.index != co2.index) Maybe.empty
        else {
          val tc  = tcs.values(co1.index).asInstanceOf[Name[F[Any]]]
          val lab = labels.values(co1.index).asInstanceOf[String]
          Maybe.just(/~\[F, L2, Any](tc.value, (lab, co1.value, co2.value)))
        }
      }
    }
  }

}
object LabelledEncoder {
  @inline def apply[F[_]](
    implicit i: LabelledEncoder[F]
  ): LabelledEncoder[F] = i
}
