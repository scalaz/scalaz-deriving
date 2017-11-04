// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

import shapeless.{ Cached, Lazy }

// invariant parent of Coproductive and Codivide
trait CoproductiveCodivide[F[_]] extends InvariantFunctor[F] {
  def xcoproduct1[Z, A1](fa: F[A1])(f: A1 => Z, g: Z => A1): F[Z] =
    xmap(fa, f, g)
  def xcoproduct2[Z, A1, A2](fb: => F[A1], fc: => F[A2])(f: A1 \/ A2 => Z,
                                                         g: Z => A1 \/ A2): F[Z]
  def xcoproduct3[Z, A1, A2, A3](fb: => F[A1], fc: => F[A2], fd: => F[A3])(
    f: A1 \/ (A2 \/ A3) => Z,
    g: Z => A1 \/ (A2 \/ A3)
  ): F[Z]
  def xcoproduct4[Z, A1, A2, A3, A4](
    fb: => F[A1],
    fc: => F[A2],
    fd: => F[A3],
    fe: => F[A4]
  )(f: A1 \/ (A2 \/ (A3 \/ A4)) => Z, g: Z => A1 \/ (A2 \/ (A3 \/ A4))): F[Z]

  def xcoderiving1[Z, A1](f: A1 => Z, g: Z => A1)(
    implicit fa: Cached[Lazy[F[A1]]]
  ): F[Z] = xcoproduct1(fa.value.value)(f, g)
  def xcoderiving2[Z, A1, A2](f: (A1 \/ A2) => Z, g: Z => (A1 \/ A2))(
    implicit fa: Cached[Lazy[F[A1]]],
    fb: Cached[Lazy[F[A2]]]
  ): F[Z] = xcoproduct2(fa.value.value, fb.value.value)(f, g)
  def xcoderiving3[Z, A1, A2, A3](f: (A1 \/ (A2 \/ A3)) => Z,
                                  g: Z => (A1 \/ (A2 \/ A3)))(
    implicit
    fa: Cached[Lazy[F[A1]]],
    fb: Cached[Lazy[F[A2]]],
    fc: Cached[Lazy[F[A3]]]
  ): F[Z] = xcoproduct3(fa.value.value, fb.value.value, fc.value.value)(f, g)
  def xcoderiving4[Z, A1, A2, A3, A4](f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z,
                                      g: Z => (A1 \/ (A2 \/ (A3 \/ A4))))(
    implicit
    fa: Cached[Lazy[F[A1]]],
    fb: Cached[Lazy[F[A2]]],
    fc: Cached[Lazy[F[A3]]],
    fd: Cached[Lazy[F[A4]]]
  ): F[Z] =
    xcoproduct4(fa.value.value, fb.value.value, fc.value.value, fd.value.value)(
      f,
      g
    )
}
object CoproductiveCodivide {
  @inline def apply[F[_]](
    implicit i: CoproductiveCodivide[F]
  ): CoproductiveCodivide[F] = i
}
