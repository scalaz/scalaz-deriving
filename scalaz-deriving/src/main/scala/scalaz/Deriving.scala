// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.{ inline, AnyRef }

import iotaz._
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

/**
 * Interface for generic derivation of typeclasses (and algebras) for products
 * and coproducts.
 *
 * Typeclass authors provide an implementation of this by extending Altz,
 * Decidablez, LabelledEncoder or LabelledDecoder, or by wrapping an existing
 * InvariantAlt instance in ExtendedInvariantAlt.
 *
 * Downstream users call this API via the DerivingMacro.
 */
trait Deriving[F[_]] {

  def xproductz[Z, L <: TList, FL <: TList, N <: TList](
    tcs: Prod[FL],
    labels: Prod[N]
  )(
    f: Prod[L] => Z,
    g: Z => Prod[L]
  )(
    implicit
    ev1: λ[a => Name[F[a]]] ƒ L ↦ FL,
    ev2: λ[a => String] ƒ L ↦ N
  ): F[Z]

  def xcoproductz[Z, L <: TList, FL <: TList, N <: TList](
    tcs: Prod[FL],
    labels: Prod[N]
  )(
    f: Cop[L] => Z,
    g: Z => Cop[L]
  )(
    implicit
    ev1: λ[a => Name[F[a]]] ƒ L ↦ FL,
    ev2: λ[a => String] ƒ L ↦ N
  ): F[Z]

}
object Deriving {
  @inline def apply[F[_]](implicit F: Deriving[F]): Deriving[F] = F

  /**
   * Generate, for a given case class, object, or sealed trait `A` a call to
   * relevant `Deriving` method to produce an `F[A]`.
   *
   * There is no magic in this macro, it is pure boilerplate generation. e.g.
   * for `case class Foo(s: String, i: Int)` and `Equal`, the following is
   * generated:
   *
   * {{{
   * val gen = ProdGen.gen[Foo]
   * val tcs = Prod(Need(implicitly[Equal[String]]), Need(implicitly[Equal[Int]]))
   * implicitly[Deriving[Equal]].xproductz(tcs, gen.labels)(gen.to, gen.from)
   * }}}
   *
   * And similarly for a sealed trait (but instead calling `CopGen.gen` and
   * `xcoproductz`).
   */
  def gen[F[_], A]: F[A] = macro macros.IotaDerivingMacros.gen[F, A]

  // hide the detail that this is an Decidablez to avoid exposing Divide
  implicit val _deriving_equal: Deriving[Equal] = new Decidablez[Equal] {
    import Scalaz._

    def productz[Z, G[_]: Traverse](f: Z =*> G): Equal[Z] = { (z1: Z, z2: Z) =>
      (z1.asInstanceOf[AnyRef].eq(z2.asInstanceOf[AnyRef])) ||
      f(z1, z2).all {
        case fa /~\ ((a1, a2)) =>
          (a1.asInstanceOf[AnyRef].eq(a2.asInstanceOf[AnyRef])) ||
            fa.equal(a1, a2)
      }
    }

    def coproductz[Z](f: Z =+> Maybe): Equal[Z] = { (z1: Z, z2: Z) =>
      (z1.asInstanceOf[AnyRef].eq(z2.asInstanceOf[AnyRef])) || f(z1, z2).map {
        case fa /~\ ((a1, a2)) =>
          (a1.asInstanceOf[AnyRef].eq(a2.asInstanceOf[AnyRef])) ||
            fa.equal(a1, a2)
      }.getOrElse(false)
    }
  }

  implicit val _deriving_show: Deriving[Show] =
    new LabelledEncoder[Show] {
      import Scalaz._

      def productz[Z, G[_]: Traverse](f: Z =*> G): Show[Z] = Show.show { z: Z =>
        "(" +: f(z).map {
          case fa /~\ ((label, a)) => label +: "=" +: fa.show(a)
        }.intercalate(",") :+ ")"
      }
      def coproductz[Z](f: Z =+> Maybe): Show[Z] = Show.show { z: Z =>
        f(z) match {
          case fa /~\ ((label, a)) => label +: fa.show(a)
        }
      }
    }

}
