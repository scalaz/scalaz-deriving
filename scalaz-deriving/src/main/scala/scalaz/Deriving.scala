// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.{ inline, Any, Boolean }
import scala.collection.immutable.Seq

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
trait Deriving[F[_]] extends DerivingProducts[F] {
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

/**
 * A weaker form of Deriving for typeclasses that can only support products,
 * e.g. Semigroup.
 */
trait DerivingProducts[F[_]] {
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

  // scalafix:off
  // hide the detail that this is an Decidablez to avoid exposing Divide
  implicit val _deriving_equal: Deriving[Equal] = new Decidablez[Equal] {
    import Scalaz._

    // since dividez is implemented, this is ignored and is just an example
    def productz[Z, H[_]: Traverse](f: Z =*> H): Equal[Z] = { (z1: Z, z2: Z) =>
      f(z1, z2).all { case fa /~\ ((a1, a2)) => fa.equal(a1, a2) }
    }

    // since choosez is implemented, this is ignored and is just an example
    def coproductz[Z](f: Z =+> Maybe): Equal[Z] = { (z1: Z, z2: Z) =>
      f(z1, z2).map { case fa /~\ ((a1, a2)) => fa.equal(a1, a2) }
        .getOrElse(false)
    }

    // if you like sausages, don't go to a sausage factory...
    override def dividez[Z, L <: TList, FL <: TList](
      tcs: Prod[FL]
    )(
      g: Z => Prod[L]
    )(
      implicit @unused ev: λ[a => Name[Equal[a]]] ƒ L ↦ FL
    ): Equal[Z] = new Equal[Z] {

      // SAM types and return keyword do not mix...
      def equal(z1: Z, z2: Z): Boolean = {
        val a1s = g(z1).values.toArray
        val a2s = g(z2).values.toArray
        val eqs = tcs.values.asInstanceOf[Seq[Name[Equal[Any]]]].toArray
        var i   = 0
        while (i < eqs.length) {
          if (!eqs(i).value.equal(a1s(i), a2s(i)))
            return false
          i += 1
        }
        true
      }
    }

    override def choosez[Z, L <: TList, FL <: TList](
      tcs: Prod[FL]
    )(
      g: Z => Cop[L]
    )(
      implicit @unused ev: λ[a => Name[Equal[a]]] ƒ L ↦ FL
    ): Equal[Z] = new Equal[Z] {
      def equal(z1: Z, z2: Z): Boolean = {
        val a1 = g(z1)
        val a2 = g(z2)

        (a1.index == a2.index) && {
          tcs.values
            .asInstanceOf[Seq[Name[Equal[Any]]]](a1.index)
            .value
            .equal(a1.value, a2.value)
        }
      }
    }
  }

  implicit val _deriving_order: Deriving[Order] = new Decidablez[Order] {
    def productz[Z, H[_]: Traverse](f: Z =*> H): Order[Z] = null
    def coproductz[Z](f: Z =+> Maybe): Order[Z]           = null

    override def dividez[Z, L <: TList, FL <: TList](
      tcs: Prod[FL]
    )(
      g: Z => Prod[L]
    )(
      implicit @unused ev: λ[a => Name[Order[a]]] ƒ L ↦ FL
    ): Order[Z] = {
      val delegate =
        _deriving_equal.asInstanceOf[Decidablez[Equal]].dividez(tcs)(g)(null)
      new Order[Z] {
        override def equal(z1: Z, z2: Z): Boolean = delegate.equal(z1, z2)
        def order(z1: Z, z2: Z): Ordering = {
          val a1s = g(z1).values.toArray
          val a2s = g(z2).values.toArray
          val eqs = tcs.values.asInstanceOf[Seq[Name[Order[Any]]]].toArray
          var i   = 0
          while (i < eqs.length) {
            val ord = eqs(i).value.order(a1s(i), a2s(i))
            if (ord != Ordering.EQ) return ord
            i += 1
          }
          Ordering.EQ
        }
      }
    }

    override def choosez[Z, L <: TList, FL <: TList](
      tcs: Prod[FL]
    )(
      g: Z => Cop[L]
    )(
      implicit @unused ev: λ[a => Name[Order[a]]] ƒ L ↦ FL
    ): Order[Z] = {
      val delegate =
        _deriving_equal.asInstanceOf[Decidablez[Equal]].choosez(tcs)(g)(null)
      new Order[Z] {
        override def equal(z1: Z, z2: Z): Boolean = delegate.equal(z1, z2)
        def order(z1: Z, z2: Z): Ordering = {
          val a1 = g(z1)
          val a2 = g(z2)

          if (a1.index != a2.index)
            return Ordering.fromInt(a1.index - a2.index)
          tcs.values
            .asInstanceOf[Seq[Name[Order[Any]]]](a1.index)
            .value
            .order(a1.value, a2.value)
        }
      }
    }
  }
  // scalafix:on

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

object DerivingProducts {
  @inline def apply[F[_]](
    implicit F: DerivingProducts[F]
  ): DerivingProducts[F] = F

  implicit val _deriving_semigroup: DerivingProducts[Semigroup] =
    new InvariantApplicativez[Semigroup] {
      // *silent crying* the iotaz API doesn't give much type safety
      override def xproductz[Z, L <: TList, FL <: TList](
        tcs: Prod[FL]
      )(
        f: Prod[L] => Z,
        g: Z => Prod[L]
      )(
        implicit @unused ev1: λ[a => Name[Semigroup[a]]] ƒ L ↦ FL
      ): Semigroup[Z] = new Semigroup[Z] {
        // can't use SAM types with by-name parameters
        def append(z1: Z, z2: =>Z): Z = {
          import scala.collection.immutable.Seq

          val els = g(z1).values
            .zip(g(z2).values)
            .zip(tcs.values.asInstanceOf[Seq[Name[Semigroup[scala.Any]]]])
            .map {
              case ((a1, a2), tc) => tc.value.append(a1, a2)
            }
          f(Prod.unsafeApply[L](els))
        }
      }
    }

  implicit val _deriving_monoid: DerivingProducts[Monoid] =
    new InvariantApplicativez[Monoid] {
      override def xproductz[Z, L <: TList, FL <: TList](
        tcs: Prod[FL]
      )(
        f: Prod[L] => Z,
        g: Z => Prod[L]
      )(
        implicit @unused ev1: λ[a => Name[Monoid[a]]] ƒ L ↦ FL
      ): Monoid[Z] = new Monoid[Z] {
        import scala.collection.immutable.Seq
        def append(z1: Z, z2: =>Z): Z = {
          val els = g(z1).values
            .zip(g(z2).values)
            .zip(tcs.values.asInstanceOf[Seq[Name[Monoid[scala.Any]]]])
            .map {
              case ((a1, a2), tc) => tc.value.append(a1, a2)
            }
          f(Prod.unsafeApply[L](els))
        }

        def zero: Z = {
          val els = tcs.values.asInstanceOf[Seq[Name[Monoid[scala.Any]]]].map {
            tc =>
              tc.value.zero
          }
          f(Prod.unsafeApply[L](els))
        }
      }
    }

  // these must be duplicated here so they are in the implicit scope. This is not
  // a problem when they are defined on the companion of the typeclass, but since
  // we are retrofitting scalaz-core, we must endure a few hacks...
  implicit val _deriving_show: DerivingProducts[Show] = Deriving._deriving_show
  implicit val _deriving_equal: DerivingProducts[Equal] =
    Deriving._deriving_equal
  implicit val _deriving_order: DerivingProducts[Order] =
    Deriving._deriving_order

}
