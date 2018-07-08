// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.inline

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
   * This is only visible to implementors, it is not part of the public API.
   * Implementors may also choose to implement xproductz directly for
   * performance reasons.
   */
  protected def coproductz[Z](
    f: (F ~> EphemeralStream) => EphemeralStream[Z]
  ): F[Z]

  // implementation...
  final protected def coproductz[Z](
    f: (F ~> EphemeralStream) => EphemeralStream[Z],
    @unused g: Z =+> Maybe
  ): F[Z] = coproductz(f)

  def altlyz[Z, L <: TList, FL <: TList](tcs: Prod[FL])(
    f: Cop[L] => Z
  )(
    implicit ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z] = xcoproductz(tcs)(f, null) // scalafix:ok
  // contravariant param is ignored

  // derived combinators
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
