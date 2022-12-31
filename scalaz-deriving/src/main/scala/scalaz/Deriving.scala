// Copyright: 2017 - 2023 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline, Any, AnyRef, Boolean }

import Scalaz._

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

  def xcoproductz[Z, A <: TList, FA <: TList](
    tcs: Prod[FA]
  )(
    f: Cop[A] => Z,
    g: Z => Cop[A]
  )(implicit
    ev: A PairedWith FA
  ): F[Z]

}

/**
 * A weaker form of Deriving for typeclasses that can only support products,
 * e.g. Semigroup.
 */
trait DerivingProducts[F[_]] {
  // convenient aliases for the API to minimise imports
  type NameF[a]         = Name[F[a]]
  type TList            = iotaz.TList
  type Prod[a <: TList] = iotaz.Prod[a]
  type Cop[a <: TList]  = iotaz.Cop[a]

  type PairedWith[A <: TList, FA <: TList] =
    iotaz.TList.Compute.Aux[iotaz.TList.Op.Map[NameF, A], FA]

  // scalafix:off DisableSyntax.implicitConversion
  // provides convenient syntax for implementors...
  import iotaz.{ Cops, Prods }
  protected implicit def ProdOps[A <: TList](p: Prod[A]): Prods.ops.ProdOps[A] =
    new Prods.ops.ProdOps[A](p)
  protected implicit def ProdOps2[A <: TList](
    p: (Prod[A], Prod[A])
  ): Prods.ops.ProdOps2[A] =
    new Prods.ops.ProdOps2[A](p)
  protected implicit def CopOps[A <: TList](p: Cop[A]): Cops.ops.CopOps[A]     =
    new Cops.ops.CopOps[A](p)
  protected implicit def CopOps2[A <: TList](
    p: (Cop[A], Cop[A])
  ): Cops.ops.CopOps2[A] =
    new Cops.ops.CopOps2[A](p)
  // scalafix:on

  def xproductz[Z, A <: TList, FA <: TList](
    tcs: Prod[FA]
  )(
    f: Prod[A] => Z,
    g: Z => Prod[A]
  )(implicit
    ev: A PairedWith FA
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
   * val iso = Prod.gen[Foo, String :: Int :: TNil]
   * val tcs = Prod(Need(Equal[String]), Need(Equal[Int]))
   * implicitly[Deriving[Equal]].xproductz(tcs)(iso.to, iso.from)
   * }}}
   *
   * And similarly for a sealed trait (but instead calling `Cop.gen` and
   * `xcoproductz`).
   */
  def gen[F[_], A]: F[A] = macro macros.IotaDerivingMacros.gen[F, A]

  private class DecidablezEqual extends Decidablez[Equal] {
    @inline private final def quick(a: Any, b: Any): Boolean =
      a.asInstanceOf[AnyRef].eq(b.asInstanceOf[AnyRef])

    override def divide2[A1, A2, Z](a1: =>Equal[A1], a2: =>Equal[A2])(
      f: Z => (A1, A2)
    ): Equal[Z] =
      divide(a1, a2)(f)

    override def dividez[Z, A <: TList, FA <: TList](tcs: Prod[FA])(
      g: Z => Prod[A]
    )(implicit
      ev: A PairedWith FA
    ): Equal[Z] = { (z1, z2) =>
      (g(z1), g(z2)).zip(tcs).all { p =>
        val (a1, a2) = p.a
        val fa       = p.b
        quick(a1, a2) || fa.value.equal(a1, a2)
      }
    }

    override def choosez[Z, A <: TList, FA <: TList](tcs: Prod[FA])(
      g: Z => Cop[A]
    )(implicit
      ev: A PairedWith FA
    ): Equal[Z] = { (z1, z2) =>
      (g(z1), g(z2))
        .zip(tcs)
        .into {
          case -\/(_) => false
          case \/-(p) =>
            val (a1, a2) = p.a
            val fa       = p.b
            quick(a1, a2) || fa.value.equal(a1, a2)
        }
    }
  }

  // hide the detail that this is an Decidablez to avoid exposing Divide
  implicit val _deriving_equal: Deriving[Equal] = new DecidablezEqual
  implicit val _deriving_order: Deriving[Order] = new Decidablez[Order] {
    @inline private final def quick(a: Any, b: Any): Boolean =
      a.asInstanceOf[AnyRef].eq(b.asInstanceOf[AnyRef])

    override def divide2[A1, A2, Z](a1: =>Order[A1], a2: =>Order[A2])(
      f: Z => (A1, A2)
    ): Order[Z] =
      divide(a1, a2)(f)

    override def dividez[Z, A <: TList, FA <: TList](tcs: Prod[FA])(
      g: Z => Prod[A]
    )(implicit
      ev: A PairedWith FA
    ): Order[Z] = {
      val delegate = new DecidablezEqual().dividez(tcs)(g)(null) // scalafix:ok
      new Order[Z] {
        override def equal(z1: Z, z2: Z): Boolean = delegate.equal(z1, z2)
        def order(z1: Z, z2: Z): Ordering         =
          (g(z1), g(z2)).zip(tcs).foldRight(Ordering.EQ: Ordering) { (p, acc) =>
            val (a1, a2) = p.a
            val fa       = p.b
            if (quick(a1, a2)) acc
            else
              fa.value.order(a1, a2) match {
                case Ordering.EQ => acc
                case ord         => ord
              }
          }
      }
    }

    override def choosez[Z, A <: TList, FA <: TList](tcs: Prod[FA])(
      g: Z => Cop[A]
    )(implicit
      ev: A PairedWith FA
    ): Order[Z] = {
      val delegate = new DecidablezEqual().choosez(tcs)(g)(null) // scalafix:ok
      new Order[Z] {
        override def equal(z1: Z, z2: Z): Boolean = delegate.equal(z1, z2)
        def order(z1: Z, z2: Z): Ordering         =
          (g(z1), g(z2)).zip(tcs).into {
            case -\/((i1, _, i2, _)) => Ordering.fromInt(i1 - i2)
            case \/-(p)              =>
              val (a1, a2) = p.a
              val fa       = p.b
              if (quick(a1, a2)) Ordering.EQ
              else fa.value.order(a1, a2)
          }
      }
    }
  }

}

object DerivingProducts {
  @inline def apply[F[_]](implicit
    F: DerivingProducts[F]
  ): DerivingProducts[F] = F

  private class InvariantApplicativezSemigroup
      extends InvariantApplicativez[Semigroup] {
    type L[a] = ((a, a), NameF[a])
    private[this] val appender = λ[L ~> Id] { case ((a1, a2), fa) =>
      fa.value.append(a1, a2)
    }

    override def xproductz[Z, A <: TList, FA <: TList](
      tcs: Prod[FA]
    )(
      f: Prod[A] => Z,
      g: Z => Prod[A]
    )(implicit
      ev: A PairedWith FA
    ): Semigroup[Z] =
      new Semigroup[Z] {
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
      override def xproductz[Z, A <: TList, FA <: TList](
        tcs: Prod[FA]
      )(
        f: Prod[A] => Z,
        g: Z => Prod[A]
      )(implicit
        ev: A PairedWith FA
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
  implicit val _deriving_equal: DerivingProducts[Equal] =
    Deriving._deriving_equal
  implicit val _deriving_order: DerivingProducts[Order] =
    Deriving._deriving_order

}
