// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String
import scala.inline

import iotaz._
import iotaz.TList.::
import Cops._

/**
 * Generic extension of Alt implementing Deriving, with a convenient API.
 */
trait InvariantAltz[F[_]]
    extends InvariantApplicativez[F]
    with InvariantAlt[F]
    with Deriving[F] {

  def xcoproductz[Z, A <: TList, TC <: TList](
    tcs: Prod[TC]
  )(
    f: Cop[A] => Z,
    g: Z => Cop[A]
  )(
    implicit ev1: NameF ƒ A ↦ TC
  ): F[Z]

  override final def xcoproductz[Z, A <: TList, TC <: TList, L <: TList](
    tcs: Prod[TC],
    @unused labels: Prod[L],
    @unused name: String
  )(
    f: Cop[A] => Z,
    g: Z => Cop[A]
  )(
    implicit
    ev1: NameF ƒ A ↦ TC,
    ev2: Label ƒ A ↦ L
  ): F[Z] = xcoproductz(tcs)(f, g)(ev1)

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
