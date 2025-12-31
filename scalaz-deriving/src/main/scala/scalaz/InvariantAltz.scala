/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package scalaz

import scala.inline
import scalaz.iotaz.*
import scalaz.iotaz.Cops.*
import scalaz.iotaz.TList.::

/**
 * Generic extension of Alt implementing Deriving.
 */
trait InvariantAltz[F[_]]
    extends InvariantApplicativez[F]
    with InvariantAlt[F]
    with Deriving[F] {

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
