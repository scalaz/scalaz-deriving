// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

import shapeless.{ Cached, Lazy }

// can be implemented by Apply and LazyDivide
// https://github.com/scalaz/scalaz/issues/1481
// may be in scalaz7.3
trait ApplyDivide[F[_]] extends InvariantFunctor[F] {

  def xproduct1[Z, A1](fa: F[A1])(f: A1 => Z, g: Z => A1): F[Z] = xmap(fa, f, g)
  def xproduct2[Z, A1, A2](fa: => F[A1], fb: => F[A2])(f: (A1, A2) => Z,
                                                       g: Z => (A1, A2)): F[Z]
  def xproduct3[Z, A1, A2, A3](fa: => F[A1], fb: => F[A2], fc: => F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z]
  def xproduct4[Z, A1, A2, A3, A4](
    fa: => F[A1],
    fb: => F[A2],
    fc: => F[A3],
    fd: => F[A4]
  )(f: (A1, A2, A3, A4) => Z, g: Z => (A1, A2, A3, A4)): F[Z]

  final def xderiving1[Z, A1](f: A1 => Z, g: Z => A1)(
    implicit fa: Cached[Lazy[F[A1]]]
  ): F[Z] = xproduct1(fa.value.value)(f, g)
  final def xderiving2[Z, A1, A2](f: (A1, A2) => Z, g: Z => (A1, A2))(
    implicit fa: Cached[Lazy[F[A1]]],
    fb: Cached[Lazy[F[A2]]]
  ): F[Z] = xproduct2(fa.value.value, fb.value.value)(f, g)
  final def xderiving3[Z, A1, A2, A3](f: (A1, A2, A3) => Z,
                                      g: Z => (A1, A2, A3))(
    implicit
    fa: Cached[Lazy[F[A1]]],
    fb: Cached[Lazy[F[A2]]],
    fc: Cached[Lazy[F[A3]]]
  ): F[Z] = xproduct3(fa.value.value, fb.value.value, fc.value.value)(f, g)
  final def xderiving4[Z, A1, A2, A3, A4](f: (A1, A2, A3, A4) => Z,
                                          g: Z => (A1, A2, A3, A4))(
    implicit
    fa: Cached[Lazy[F[A1]]],
    fb: Cached[Lazy[F[A2]]],
    fc: Cached[Lazy[F[A3]]],
    fd: Cached[Lazy[F[A4]]]
  ): F[Z] =
    xproduct4(fa.value.value, fb.value.value, fc.value.value, fd.value.value)(f,
                                                                              g)
}
object ApplyDivide {
  @inline def apply[F[_]](implicit i: ApplyDivide[F]): ApplyDivide[F] = i
}
