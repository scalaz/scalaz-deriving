// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.{ inline, Any }
import scala.collection.immutable.{ List, Seq }

import iotaz._
import iotaz.TList.::
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

import Scalaz._
import Prods._

/**
 * Generic extension of InvariantApplicative implementing DerivingProducts, with a convenient API.
 */
trait InvariantApplicativez[F[_]]
    extends InvariantApplicative[F]
    with DerivingProducts[F] {
  type G[_]
  def G: Applicative[G]

  type =*>[Z, H[_]] = ArityExists[Z, F, H]
  type =+>[Z, H[_]] = ArityExists1[Z, F, H]

  /**
   * This is only visible to implementors, it is not part of the public API.
   * Implementors may also choose to implement xproductz directly for
   * performance reasons.
   */
  protected def productz[Z, H[_]: Traverse](
    f: (F ~> G) => G[Z],
    g: Z =*> H
  ): F[Z]

  override final def xproductz[Z, L <: TList, FL <: TList, N <: TList](
    tcs: Prod[FL],
    @unused labels: Prod[N]
  )(
    f: Prod[L] => Z,
    g: Z => Prod[L]
  )(
    implicit
    ev1: λ[a => Name[F[a]]] ƒ L ↦ FL,
    ev2: λ[a => String] ƒ L ↦ N
  ): F[Z] = xproductz(tcs)(f, g)(ev1)

  def xproductz[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Prod[L] => Z,
    g: Z => Prod[L]
  )(
    implicit
    @unused ev1: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = {
    import /~\.T2

    val fz = { (faa: F ~> G) =>
      implicit val GA: Applicative[G] = G
      tcs.values
        .asInstanceOf[Seq[Name[F[Any]]]] // from ev
        .toList
        .traverse(nty => faa(nty.value))
        .map(v => f(Prod.unsafeApply(v)))
    }

    val gz = {
      new (Z =*> List) {
        override def apply(z: Z): List[F /~\ Id] =
          g(z).values
            .zip(tcs.values)
            .map { tcv =>
              /~\[F, Id, Any](tcv._2.asInstanceOf[Name[F[Any]]].value, tcv._1)
            }(scala.collection.breakOut)
        override def apply(z1: Z, z2: Z): List[F /~\ T2] =
          g(z1).values
            .zip(g(z2).values)
            .zip(tcs.values)
            .map { tcv =>
              val ((v1, v2), tc) = tcv
              /~\[F, T2, Any](tc.asInstanceOf[Name[F[Any]]].value, (v1, v2))
            }(scala.collection.breakOut)
      }
    }

    productz[Z, List](fz, gz)
  }

  override def xmap[A, B](ma: F[A], f: A => B, g: B => A): F[B] =
    xproduct1(ma)(f, g)

  override def xproduct0[Z](z: =>Z): F[Z] =
    xproductz[Z, TNil, TNil](empty)(_ => z, _ => empty)
  override def xproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z] = {
    type L = A1 :: TNil
    xproductz(LazyProd(a1))(
      (a: Prod[L]) => f(to1T(a)),
      z => Prod[A1 :: TNil](g(z))
    )
  }
  override def xproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: (A1, A2) => Z,
    g: Z => (A1, A2)
  ): F[Z] = {
    type L = A1 :: A2 :: TNil
    xproductz(LazyProd(a1, a2))(
      (as: Prod[L]) => f.tupled(to2T(as)),
      z => from2T(g(z))
    )
  }
  override def xproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: TNil
    xproductz(LazyProd(a1, a2, a3))(
      (as: Prod[L]) => f.tupled(to3T(as)),
      z => from3T(g(z))
    )
  }
  override def xproduct4[Z, A1, A2, A3, A4](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3],
    a4: =>F[A4]
  )(
    f: (A1, A2, A3, A4) => Z,
    g: Z => (A1, A2, A3, A4)
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: A4 :: TNil
    xproductz(LazyProd(a1, a2, a3, a4))(
      (as: Prod[L]) => f.tupled(to4T(as)),
      z => from4T(g(z))
    )
  }

}
object InvariantApplicativez {
  @inline def apply[F[_]](
    implicit i: InvariantApplicativez[F]
  ): InvariantApplicativez[F] = i
}
