// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

import shapeless.{ Cached, Lazy }

// can be implemented by Apply and LazyDivide
// https://github.com/scalaz/scalaz/issues/1481
// may be in scalaz7.3
trait ApplyDivide[F[_]] extends InvariantFunctor[F] {

  def xproduct1[Z, A1](a1: F[A1])(f: A1 => Z, g: Z => A1): F[Z] = xmap(a1, f, g)
  def xproduct2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: (A1, A2) => Z,
                                                       g: Z => (A1, A2)): F[Z]
  def xproduct3[Z, A1, A2, A3](a1: => F[A1], a2: => F[A2], a3: => F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z]
  def xproduct4[Z, A1, A2, A3, A4](
    a1: => F[A1],
    a2: => F[A2],
    a3: => F[A3],
    a4: => F[A4]
  )(f: (A1, A2, A3, A4) => Z, g: Z => (A1, A2, A3, A4)): F[Z]

  final def xderiving1[Z, A1](f: A1 => Z, g: Z => A1)(
    implicit a1: Cached[Lazy[F[A1]]]
  ): F[Z] = xproduct1(a1.value.value)(f, g)
  final def xderiving2[Z, A1, A2](f: (A1, A2) => Z, g: Z => (A1, A2))(
    implicit a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]]
  ): F[Z] = xproduct2(a1.value.value, a2.value.value)(f, g)
  final def xderiving3[Z, A1, A2, A3](f: (A1, A2, A3) => Z,
                                      g: Z => (A1, A2, A3))(
    implicit
    a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]]
  ): F[Z] = xproduct3(a1.value.value, a2.value.value, a3.value.value)(f, g)
  final def xderiving4[Z, A1, A2, A3, A4](f: (A1, A2, A3, A4) => Z,
                                          g: Z => (A1, A2, A3, A4))(
    implicit
    a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]],
    a4: Cached[Lazy[F[A4]]]
  ): F[Z] =
    xproduct4(a1.value.value, a2.value.value, a3.value.value, a4.value.value)(f,
                                                                              g)
}
object ApplyDivide {
  @inline def apply[F[_]](implicit i: ApplyDivide[F]): ApplyDivide[F] = i
}
