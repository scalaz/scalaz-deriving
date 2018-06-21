// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

import iotaz._
import iotaz.TList.Compute.{ Aux => ↦ }
import iotaz.TList.Op.{ Map => ƒ }

/** Implementation of Decidable in terms of a single, generic, method. */
trait Decidablez[F[_]] extends Decidable[F] {
  import Cops._

  def choosez[Z, L <: TList, FL <: TList](
    tcs: Prod[FL]
  )(
    f: Z => Cop[L]
  )(
    implicit
    ev: λ[a => Name[F[a]]] ƒ L ↦ FL
  ): F[Z]

  override def choose1[Z, A1](a1: =>F[A1])(f: Z => A1): F[Z] =
    choosez(LazyProd(a1))(z => from1(f(z)))
  override def choose2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: Z => A1 \/ A2
  ): F[Z] =
    choosez(LazyProd(a1, a2))(z => from2(f(z)))
  override def choose3[Z, A1, A2, A3](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3]
  )(f: Z => A1 \/ (A2 \/ A3)): F[Z] =
    choosez(LazyProd(a1, a2, a3))(z => from3(f(z)))
  override def choose4[Z, A1, A2, A3, A4](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3],
    a4: =>F[A4]
  )(f: Z => A1 \/ (A2 \/ (A3 \/ A4))): F[Z] =
    choosez(LazyProd(a1, a2, a3, a4))(z => from4(f(z)))

}
object Decidablez {
  @inline def apply[F[_]](implicit i: Decidablez[F]): Decidablez[F] = i
}
