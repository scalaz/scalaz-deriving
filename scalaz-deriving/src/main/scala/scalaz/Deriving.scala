// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String
import scala.{ inline, Any, AnyRef, Boolean }

import Scalaz._
import org.scalacheck.{ Arbitrary, Gen }

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

  def xcoproductz[Z, A <: TList, TC <: TList, L <: TList](
    tcs: Prod[TC],
    labels: Prod[L],
    name: String
  )(
    f: Cop[A] => Z,
    g: Z => Cop[A]
  )(
    implicit
    ev1: NameF ƒ A ↦ TC,
    ev2: Label ƒ A ↦ L
  ): F[Z]

}

/**
 * A weaker form of Deriving for typeclasses that can only support products,
 * e.g. Semigroup.
 */
trait DerivingProducts[F[_]] {
  // convenient aliases for the API to minimise imports
  type NameF[a]                  = Name[F[a]]
  type Label[a]                  = iotaz.Prods.Label[a]
  type TList                     = iotaz.TList
  type Prod[a <: TList]          = iotaz.Prod[a]
  type Cop[a <: TList]           = iotaz.Cop[a]
  type ƒ[f[_], a <: TList]       = iotaz.TList.Op.Map[f, a]
  type ↦[a <: TList, o <: TList] = iotaz.TList.Compute.Aux[a, o]

  // scalafix:off DisableSyntax.implicitConversion
  // provides convenient syntax for implementors...
  import iotaz.{ Cops, Prods }
  protected implicit def ProdOps[A <: TList](p: Prod[A]): Prods.ops.ProdOps[A] =
    new Prods.ops.ProdOps[A](p)
  protected implicit def ProdOps2[A <: TList](
    p: (Prod[A], Prod[A])
  ): Prods.ops.ProdOps2[A] =
    new Prods.ops.ProdOps2[A](p)
  protected implicit def CopOps[A <: TList](p: Cop[A]): Cops.ops.CopOps[A] =
    new Cops.ops.CopOps[A](p)
  protected implicit def CopOps2[A <: TList](
    p: (Cop[A], Cop[A])
  ): Cops.ops.CopOps2[A] =
    new Cops.ops.CopOps2[A](p)
  // scalafix:on

  def xproductz[Z, A <: TList, TC <: TList, L <: TList](
    tcs: Prod[TC],
    labels: Prod[L],
    name: String
  )(
    f: Prod[A] => Z,
    g: Z => Prod[A]
  )(
    implicit
    ev1: NameF ƒ A ↦ TC,
    ev2: Label ƒ A ↦ L
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

  private class DecidablezEqual extends Decidablez[Equal] {
    @inline private final def quick(a: Any, b: Any): Boolean =
      a.asInstanceOf[AnyRef].eq(b.asInstanceOf[AnyRef])

    override def dividez[Z, A <: TList, TC <: TList](
      tcs: Prod[TC]
    )(
      g: Z => Prod[A]
    )(
      implicit ev: NameF ƒ A ↦ TC
    ): Equal[Z] = { (z1, z2) =>
      (g(z1), g(z2)).zip(tcs).foldRight(true) {
        case ((a1, a2) /~\ fa, acc) =>
          (quick(a1, a2) || fa.value.equal(a1, a2)) && acc
      }
    }

    override def choosez[Z, A <: TList, TC <: TList](
      tcs: Prod[TC]
    )(
      g: Z => Cop[A]
    )(
      implicit ev: NameF ƒ A ↦ TC
    ): Equal[Z] = { (z1, z2) =>
      (g(z1), g(z2))
        .zip(tcs)
        .into {
          case -\/(_)               => false
          case \/-((a1, a2) /~\ fa) => quick(a1, a2) || fa.value.equal(a1, a2)
        }
    }
  }

  // hide the detail that this is an Decidablez to avoid exposing Divide
  implicit val _deriving_equal: Deriving[Equal] = new DecidablezEqual
  implicit val _deriving_order: Deriving[Order] = new Decidablez[Order] {
    @inline private final def quick(a: Any, b: Any): Boolean =
      a.asInstanceOf[AnyRef].eq(b.asInstanceOf[AnyRef])

    override def dividez[Z, A <: TList, TC <: TList](
      tcs: Prod[TC]
    )(
      g: Z => Prod[A]
    )(
      implicit ev: NameF ƒ A ↦ TC
    ): Order[Z] = {
      val delegate = new DecidablezEqual().dividez(tcs)(g)(null) // scalafix:ok
      new Order[Z] {
        override def equal(z1: Z, z2: Z): Boolean = delegate.equal(z1, z2)
        def order(z1: Z, z2: Z): Ordering =
          (g(z1), g(z2)).zip(tcs).foldRight(Ordering.EQ: Ordering) {
            case ((a1, a2) /~\ fa, acc) =>
              if (quick(a1, a2)) acc
              else
                fa.value.order(a1, a2) match {
                  case Ordering.EQ => acc
                  case ord         => ord
                }
          }
      }
    }

    override def choosez[Z, A <: TList, TC <: TList](
      tcs: Prod[TC]
    )(
      g: Z => Cop[A]
    )(
      implicit ev: NameF ƒ A ↦ TC
    ): Order[Z] = {
      val delegate = new DecidablezEqual().choosez(tcs)(g)(null) // scalafix:ok
      new Order[Z] {
        override def equal(z1: Z, z2: Z): Boolean = delegate.equal(z1, z2)
        def order(z1: Z, z2: Z): Ordering =
          (g(z1), g(z2)).zip(tcs).into {
            case -\/((i1, _, i2, _)) => Ordering.fromInt(i1 - i2)
            case \/-((a1, a2) /~\ fa) =>
              if (quick(a1, a2)) Ordering.EQ
              else fa.value.order(a1, a2)
          }
      }
    }
  }

  implicit val _deriving_show: Deriving[Show] =
    new Deriving[Show] {
      def xproductz[Z, A <: TList, TC <: TList, L <: TList](
        tcs: Prod[TC],
        labels: Prod[L],
        name: String
      )(
        @unused f: Prod[A] => Z,
        g: Z => Prod[A]
      )(
        implicit
        ev1: NameF ƒ A ↦ TC,
        ev2: Label ƒ A ↦ L
      ): Show[Z] = Show.show { (z: Z) =>
        val bits = g(z).zip(tcs, labels).map {
          case (label, a) /~\ fa => label +: "=" +: fa.value.show(a)
        }
        name +: "(" +: bits.intercalate(",") :+ ")"
      }

      def xcoproductz[Z, A <: TList, TC <: TList, L <: TList](
        tcs: Prod[TC],
        labels: Prod[L],
        @unused name: String
      )(
        @unused f: Cop[A] => Z,
        g: Z => Cop[A]
      )(
        implicit
        ev1: NameF ƒ A ↦ TC,
        ev2: Label ƒ A ↦ L
      ): Show[Z] = Show.show { z =>
        g(z).zip(tcs, labels).into {
          case (_, a) /~\ fa => fa.value.show(a)
        }
      }

    }

  // we can't have an Alt because it breaks derived combinator RT... the first
  // element of a coproduct is always weighted more heavily. We could, however,
  // have an Applicativez but that would break typeclass coherence with the
  // instance in scalaz.scalacheck.ScalaCheckBinding
  implicit val _deriving_arbitrary: Deriving[Arbitrary] =
    new Deriving[Arbitrary] {
      import scalaz.scalacheck.ScalaCheckBinding._

      private val pick = λ[NameF ~> Gen](a => Gen.lzy(a.value.arbitrary))
      def xproductz[Z, A <: TList, TC <: TList, L <: TList](
        tcs: Prod[TC],
        labels: Prod[L],
        @unused name: String
      )(
        f: Prod[A] => Z,
        g: Z => Prod[A]
      )(
        implicit
        ev1: NameF ƒ A ↦ TC,
        ev2: Label ƒ A ↦ L
      ): Arbitrary[Z] =
        Arbitrary(tcs.traverse(pick).map(f))

      private val always = λ[NameF ~> λ[α => Maybe[Gen[α]]]](
        a => Maybe.just(Gen.lzy(a.value.arbitrary))
      )
      def xcoproductz[Z, A <: TList, TC <: TList, L <: TList](
        tcs: Prod[TC],
        labels: Prod[L],
        @unused name: String
      )(
        f: Cop[A] => Z,
        g: Z => Cop[A]
      )(
        implicit
        ev1: NameF ƒ A ↦ TC,
        ev2: Label ƒ A ↦ L
      ): Arbitrary[Z] = Arbitrary {
        Gen.frequency(
          tcs
            .coptraverse(always)
            .toList
            .map(g => (1, g.map(f))): _*
        )
      }
    }

}

object DerivingProducts {
  @inline def apply[F[_]](
    implicit F: DerivingProducts[F]
  ): DerivingProducts[F] = F

  private class InvariantApplicativezSemigroup
      extends InvariantApplicativez[Semigroup] {
    type L[a] = ((a, a), NameF[a])
    private val appender = λ[L ~> Id] {
      case ((a1, a2), fa) => fa.value.append(a1, a2)
    }

    override def xproductz[Z, A <: TList, TC <: TList](
      tcs: Prod[TC]
    )(
      f: Prod[A] => Z,
      g: Z => Prod[A]
    )(
      implicit ev1: NameF ƒ A ↦ TC
    ): Semigroup[Z] = new Semigroup[Z] {
      // can't use SAM types with by-name parameters
      // z2 is eagerly evaluated :-(
      def append(z1: Z, z2: =>Z): Z =
        f(tcs.ziptraverse2(g(z1), g(z2), appender))
    }
  }

  implicit val _deriving_semigroup: DerivingProducts[Semigroup] =
    new InvariantApplicativezSemigroup

  implicit val _deriving_monoid: DerivingProducts[Monoid] =
    new InvariantApplicativez[Monoid] {
      override def xproductz[Z, A <: TList, TC <: TList](
        tcs: Prod[TC]
      )(
        f: Prod[A] => Z,
        g: Z => Prod[A]
      )(
        implicit ev1: NameF ƒ A ↦ TC
      ): Monoid[Z] = {
        val delegate = new InvariantApplicativezSemigroup()
          .xproductz(tcs)(f, g)(null) // scalafix:ok

        val nada = λ[NameF ~> Id](_.value.zero)

        new Monoid[Z] {
          def append(z1: Z, z2: =>Z): Z = delegate.append(z1, z2)
          def zero: Z                   = f(tcs.traverse(nada))
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
  implicit val _deriving_arbitrary: DerivingProducts[Arbitrary] =
    Deriving._deriving_arbitrary

}
