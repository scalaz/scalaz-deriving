// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline, Any }

import iotaz._
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

import Scalaz._

/**
 * Typeclass Derivation for products, coproducts and AnyVal.
 *
 * Typeclasses with parameters in contravariant position (e.g. encoders,
 * comparators) should implement this typeclass with ContravariantDerivez.
 *
 * Typeclasses with parameters in covariant position (e.g. decoders, data
 * generators) should implement this typeclass with CovariantDerivez.
 *
 * Typeclasses with a mix of contravariant and covariant position methods (e.g.
 * a "format" that combines an encoder and a decoder) may implement this
 * typeclass directly but such constructs are usually best split into two parts,
 * with an implicit to create the combination where required.
 */
trait Derivez[F[_]] extends Derives[F] {
  // although we don't leave the G as a free parameter, it is a useful reminder
  // to the author of the instance of what they receive after providing a Z.
  type =*>[Z, G[_]] = ArityExists[Z, F, G]
  type =+>[Z, G[_]] = ArityExists1[Z, F, G]

  def xproductz[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Prod[L] => Z,
    g: Z => Prod[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z]

  def xcoproductz[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Cop[L] => Z,
    g: Z => Cop[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z]
}
object Derivez {
  @inline def apply[F[_]](
    implicit i: Derivez[F]
  ): Derivez[F] = i

  /**
   * Generate, for a given case class, object, or sealed trait `A` a call to
   * relevant `Derivez` method to produce an `F[A]`.
   *
   * There is no magic in this macro, it is pure boilerplate generation. e.g.
   * for `case class Foo(s: String, i: Int)` and `Equal`, the following is
   * generated:
   *
   * {{{
   * val gen = ProdGen.gen[Foo]
   * val tcs = Prod(Need(implicitly[Equal[String]]), Need(implicitly[Equal[Int]]))
   * Derivez.xproductz(tcs)(gen.to, gen.from)
   * }}}
   *
   * And similarly for a sealed trait (but instead calling `CopGen.gen` and
   * `xcoproductz`).
   */
  def gen[F[_], A]: F[A] = macro DerivezMacros.gen[F, A]

  // should really be on the companion of Equal
  implicit val Equal: ContravariantDerivez[Equal] =
    new ContravariantDerivez[Equal] {
      def productz[Z, G[_]: Foldable](f: Z =*> G): Equal[Z] = {
        (z1: Z, z2: Z) =>
          f(z1, z2).all { case fa /~\ ((a1, a2)) => fa.equal(a1, a2) }
      }

      def coproductz[Z](f: Z =+> Maybe): Equal[Z] = { (z1: Z, z2: Z) =>
        f(z1, z2).map { case fa /~\ ((a1, a2)) => fa.equal(a1, a2) }
          .getOrElse(false)
      }
    }

}

abstract class ContravariantDerivez[F[_]]
    extends Derivez[F]
    with Codividez[F]
    with Divisiblez[F] {

  def productz[Z, G[_]: Foldable](f: Z =*> G): F[Z]
  def coproductz[Z](f: Z =+> Maybe): F[Z]

  final def xproductz[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Prod[L] => Z,
    g: Z => Prod[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = dividez(tcs)(g)

  final def dividez[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Z => Prod[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = productz {
    import /~\.T2
    import scala.collection.immutable.List
    new (Z =*> List) {
      def apply(z: Z): List[F /~\ Id] =
        (f(z).values zip tcs.values).map { tcv =>
          /~\(tcv._2.asInstanceOf[Name[F[Any]]].value, tcv._1)
        }(scala.collection.breakOut)
      def apply(z1: Z, z2: Z): List[F /~\ T2] =
        (f(z1).values zip f(z2).values zip tcs.values).map { tcv =>
          val ((v1, v2), tc) = tcv
          /~\(tc.asInstanceOf[Name[F[Any]]].value, v1, v2)
        }(scala.collection.breakOut)
    }
  }

  final def xcoproductz[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Cop[L] => Z,
    g: Z => Cop[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = codividez(tcs)(g)

  final def codividez[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Z => Cop[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = coproductz {
    import /~\.T2
    new (Z =+> Maybe) {
      def apply(z: Z): F /~\ Id = {
        val co = f(z)
        val tc = tcs.values(co.index).asInstanceOf[Name[F[Any]]]
        /~\(tc.value, co.value)
      }
      def apply(z1: Z, z2: Z): Maybe[F /~\ T2] = {
        val co1 = f(z1)
        val co2 = f(z2)
        if (co1.index != co2.index) Maybe.empty
        else {
          val tc = tcs.values(co1.index).asInstanceOf[Name[F[Any]]]
          Maybe.just(/~\(tc.value, co1.value, co2.value))
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
    extends Derivez[F]
    with Coapplicativez[F]
    with Applicativez[F] {

  def productz[Z](f: (F ~> Id) => Z): F[Z]
  def coproductz[Z](f: (F ~> G) => G[Z]): F[Z]

  final def xproductz[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Prod[L] => Z,
    g: Z => Prod[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = applyz(tcs)(f)

  final def applyz[Z, L <: TList, FL <: TList](tcs: Prod[FL])(
    f: Prod[L] => Z
  )(
    implicit ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] =
    productz(((faa: (F ~> Id)) => f(Prods.map(tcs)(faa))))

  final def xcoproductz[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Cop[L] => Z,
    g: Z => Cop[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = coapplyz(tcs)(f)

  final def coapplyz[Z, L <: TList, FL <: TList](tcs: Prod[FL])(
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
