// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline, Any }
import scala.collection.immutable.Seq

import iotaz._
import iotaz.TList.::
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

import Cops._

/**
 * Generic extension of Alt implementing Deriving, with a convenient API.
 */
trait Altz[F[_]] extends Applicativez[F] with Alt[F] with InvariantAltz[F] {

  /**
   * Implementors can implement this or override altlyz.
   *
   * This method adds some type safety to the raw iotaz API, at a small
   * performance penalty and incurring extra restrictions.
   */
  protected def coproductz[Z](
    f: (F ~> EphemeralStream) => EphemeralStream[Z]
  ): F[Z]

  /** Implementors may override this, subject to limitations of the iotaz API. */
  def altlyz[Z, L <: TList, FL <: TList](tcs: Prod[FL])(
    f: Cop[L] => Z
  )(
    implicit @unused ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = {
    import Scalaz._

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
    coproductz(fz)
  }

  // derived combinators
  override final def xcoproductz[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Cop[L] => Z,
    @unused g: Z => Cop[L]
  )(
    implicit ev1: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = altlyz(tcs)(f)

  override def alt[A](a1: =>F[A], a2: =>F[A]): F[A] = altly2(a1, a2) {
    case -\/(a) => a
    case \/-(a) => a
  }
  override def altly1[Z, A1](a1: =>F[A1])(f: A1 => Z): F[Z] = {
    type L = A1 :: TNil
    altlyz(LazyProd(a1))((c: Cop[L]) => f(to1(c)))
  }
  override def altly2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: A1 \/ A2 => Z
  ): F[Z] = {
    type L = A1 :: A2 :: TNil
    altlyz(LazyProd(a1, a2))((c: Cop[L]) => f(to2(c)))
  }
  override def altly3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: A1 \/ (A2 \/ A3) => Z
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: TNil
    altlyz(LazyProd(a1, a2, a3))((c: Cop[L]) => f(to3(c)))
  }
  override def altly4[Z, A1, A2, A3, A4](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3],
    a4: =>F[A4]
  )(
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z
  ): F[Z] = {
    type L = A1 :: A2 :: A3 :: A4 :: TNil
    altlyz(LazyProd(a1, a2, a3, a4))((c: Cop[L]) => f(to4(c)))
  }

}
object Altz {
  @inline def apply[F[_]](implicit i: Altz[F]): Altz[F] = i
}
