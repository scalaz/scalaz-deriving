// Copyright: 2017 - 2023 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.inline

import iotaz._
import Cops._

/**
 * Generic extension of Decidable implementing Deriving.
 */
trait Decidablez[F[_]]
    extends Divisiblez[F]
    with InvariantAltz[F]
    with Decidable[F] {

  def choosez[Z, A <: TList, FA <: TList](tcs: Prod[FA])(g: Z => Cop[A])(
    implicit ev: A PairedWith FA
  ): F[Z]

  // derived combinators
  override final def xcoproductz[Z, A <: TList, FA <: TList](
    tcs: Prod[FA]
  )(
    @unused f: Cop[A] => Z,
    g: Z => Cop[A]
  )(implicit
    ev: A PairedWith FA
  ): F[Z] = choosez(tcs)(g)

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
