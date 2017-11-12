// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }
import scala.collection.immutable.Stream

import iotaz._
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

import Scalaz._

/**
 * Typeclass Derivation for products, coproducts and AnyVal.
 *
 * Typeclasses with parameters in contravariant position (e.g. encoders,
 * comparators) should implement this typeclass with ContravariantDerivedz.
 *
 * Typeclasses with parameters in covariant position (e.g. decoders, data
 * generators) should implement this typeclass with CovariantDerivedz.
 *
 * Typeclasses with a mix of contravariant and covariant position methods (e.g.
 * a "format" that combines an encoder and a decoder) may implement this
 * typeclass directly but such constructs are usually best split into two parts,
 * with an implicit to create the combination where required.
 */
trait Derivedz[F[_]] extends Derived[F] {

  // TODO: the invariant generic API

}
object Derivedz {
  @inline def apply[F[_]](
    implicit i: Derivedz[F]
  ): Derivedz[F] = i

  import Scalaz._
  import Maybe.Just

  // should really be on the companion of Equal
  implicit val Equal: Derivedz[Equal] =
    new ContravariantDerivedz[Equal] {
      def products[Z](f: Z => ProductX[Equal]): Equal[Z] = { (z1: Z, z2: Z) =>
        ProductX.and(f)(z1, z2).all {
          case (p1, p2) => p1.tc.equal(p1.value, p2.value)
        }
      }

      def coproducts[Z](f: Z => CoproductX[Equal]): Equal[Z] = {
        (z1: Z, z2: Z) =>
          CoproductX.and(f)(z1, z2) match {
            case Just((p1, p2)) => p1.tc.equal(p1.value, p2.value)
            case _              => false
          }
      }
    }

}

trait ContravariantDerivedz[F[_]]
    extends Derivedz[F]
    with Codividez[F]
    with Divisiblez[F] {

  // FIXME in light of the covariant case, can we remove ProductX / CoproductX
  // from the API and deal in existentials?
  def coproducts[Z](f: Z => CoproductX[F]): F[Z]
  def products[Z](f: Z => ProductX[F]): F[Z]

  final def codivideX[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Z => Cop[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = coproducts { z =>
    val co = f(z)
    val tc = tcs.values(co.index).asInstanceOf[Name[F[scala.Any]]]
    val v  = co.value
    CoproductX(co.index, ParamX(v, tc.value))
  }

  final def divideX[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Z => Prod[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = products { z =>
    ProductX(
      IList.fromList((f(z).values zip tcs.values).map { tcv =>
        ParamX(tcv._1, tcv._2.asInstanceOf[Name[F[scala.Any]]].value)
      }(scala.collection.breakOut))
    )
  }

}
object ContravariantDerivedz {
  @inline def apply[F[_]](
    implicit i: ContravariantDerivedz[F]
  ): ContravariantDerivedz[F] = i
}

trait CovariantDerivedz[F[_]]
    extends Derivedz[F]
    with Coapplicativez[F]
    with Applicativez[F] {

  // EphemeralStream gave stack overflow
  def coproducts[Z](f: (F ~> Maybe) => Stream[Z]): F[Z]
  def products[Z](f: (F ~> Id) => Z): F[Z]

  final def coapplyX[Z, L <: TList, FL <: TList](tcs: Prod[FL])(
    f: Cop[L] => Z
  )(
    implicit ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] =
    coproducts((faa: (F ~> Maybe)) => Cops.mapMaybe(tcs)(faa).map(f))

  final def applyX[Z, L <: TList, FL <: TList](tcs: Prod[FL])(
    f: Prod[L] => Z
  )(
    implicit ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] =
    products(((faa: (F ~> Id)) => f(Prods.map(tcs)(faa))))

}
object CovariantDerivedz {
  @inline def apply[F[_]](
    implicit i: CovariantDerivedz[F]
  ): CovariantDerivedz[F] = i
}
