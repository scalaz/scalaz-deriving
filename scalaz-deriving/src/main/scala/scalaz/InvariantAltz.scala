// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import scala.{ inline, Any }
import scala.collection.immutable.Seq

import iotaz._
import iotaz.TList.::
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

import Scalaz._
import Cops._

/**
 * Generic extension of Alt implementing Deriving, with a convenient API.
 */
trait InvariantAltz[F[_]]
    extends InvariantApplicativez[F]
    with InvariantAlt[F]
    with Deriving[F] {

  /**
   * This is only visible to implementors, it is not part of the public API.
   * Implementors may also choose to implement xproductz directly for
   * performance reasons.
   */
  protected def coproductz[Z](
    f: (F ~> EphemeralStream) => EphemeralStream[Z],
    g: Z =+> Maybe
  ): F[Z]

  override final def xcoproductz[Z, L <: TList, FL <: TList, N <: TList](
    tcs: Prod[FL],
    @unused labels: Prod[N]
  )(
    f: Cop[L] => Z,
    g: Z => Cop[L]
  )(
    implicit
    ev1: λ[a => Name[F[a]]] ƒ L ↦ FL,
    ev2: λ[a => String] ƒ L ↦ N
  ): F[Z] = xcoproductz(tcs)(f, g)(ev1)

  def xcoproductz[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Cop[L] => Z,
    g: Z => Cop[L]
  )(
    implicit
    @unused ev1: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = {
    val fz = { (faa: F ~> EphemeralStream) =>
      tcs.values
        .asInstanceOf[Seq[Name[F[Any]]]] // from TMap
        .toList
        .indexed
        .toEphemeralStream // FromFoldable would allow abstraction
        .flatMap {
          case (i, nt) =>
            val t: F[Any]                = nt.value
            val ys: EphemeralStream[Any] = faa(t)
            ys.map(y => (i, y)) // from implied InjectL
        }
        .map { case (i, y) => f(Cop.unsafeApply(i, y)) }
    }

    import /~\.T2
    val gz = new (Z =+> Maybe) {
      def apply(z: Z): F /~\ Id = {
        val co = g(z)
        val tc = tcs.values(co.index).asInstanceOf[Name[F[Any]]]
        /~\[F, Id, Any](tc.value, co.value)
      }
      def apply(z1: Z, z2: Z): Maybe[F /~\ T2] = {
        val co1 = g(z1)
        val co2 = g(z2)
        if (co1.index != co2.index) Maybe.empty
        else {
          val tc = tcs.values(co1.index).asInstanceOf[Name[F[Any]]]
          Maybe.just(/~\[F, T2, Any](tc.value, (co1.value, co2.value)))
        }
      }
    }

    coproductz(fz, gz)
  }

  override def xcoproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z] = {
    type L = A1 :: TNil
    xcoproductz(LazyProd(a1))((c: Cop[L]) => f(to1(c)), z => from1(g(z)))
  }
  override def xcoproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: A1 \/ A2 => Z,
    g: Z => A1 \/ A2
  ): F[Z] = {
    type L = A1 :: A2 :: TNil
    xcoproductz(LazyProd(a1, a2))((c: Cop[L]) => f(to2(c)), z => from2(g(z)))
  }
  override def xcoproduct3[Z, A1, A2, A3](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3]
  )(
    f: A1 \/ (A2 \/ A3) => Z,
    g: Z => A1 \/ (A2 \/ A3)
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: TNil
    xcoproductz(LazyProd(a1, a2, a3))(
      (c: Cop[L]) => f(to3(c)),
      z => from3(g(z))
    )
  }
  override def xcoproduct4[Z, A1, A2, A3, A4](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3],
    a4: =>F[A4]
  )(
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z,
    g: Z => A1 \/ (A2 \/ (A3 \/ A4))
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: A4 :: TNil
    xcoproductz(LazyProd(a1, a2, a3, a4))(
      (c: Cop[L]) => f(to4(c)),
      z => from4(g(z))
    )
  }

}
object InvariantAltz {
  @inline def apply[F[_]](implicit i: InvariantAltz[F]): InvariantAltz[F] = i
}
