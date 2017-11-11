// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

import shapeless.{ Cached, Lazy }

// invariant parent of Coapplicative and Codivide
trait CoapplicativeCodivide[F[_]] extends InvariantFunctor[F] {
  def xcoproduct1[Z, A1](a1: F[A1])(f: A1 => Z, g: Z => A1): F[Z] =
    xmap(a1, f, g)
  def xcoproduct2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: A1 \/ A2 => Z,
                                                         g: Z => A1 \/ A2): F[Z]
  def xcoproduct3[Z, A1, A2, A3](a1: => F[A1], a2: => F[A2], a3: => F[A3])(
    f: A1 \/ (A2 \/ A3) => Z,
    g: Z => A1 \/ (A2 \/ A3)
  ): F[Z]
  def xcoproduct4[Z, A1, A2, A3, A4](
    a1: => F[A1],
    a2: => F[A2],
    a3: => F[A3],
    a4: => F[A4]
  )(f: A1 \/ (A2 \/ (A3 \/ A4)) => Z, g: Z => A1 \/ (A2 \/ (A3 \/ A4))): F[Z]

  def xcoderiving1[Z, A1](f: A1 => Z, g: Z => A1)(
    implicit a1: Cached[Lazy[F[A1]]]
  ): F[Z] = xcoproduct1(a1.value.value)(f, g)
  def xcoderiving2[Z, A1, A2](f: (A1 \/ A2) => Z, g: Z => (A1 \/ A2))(
    implicit a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]]
  ): F[Z] = xcoproduct2(a1.value.value, a2.value.value)(f, g)
  def xcoderiving3[Z, A1, A2, A3](f: (A1 \/ (A2 \/ A3)) => Z,
                                  g: Z => (A1 \/ (A2 \/ A3)))(
    implicit
    a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]]
  ): F[Z] = xcoproduct3(a1.value.value, a2.value.value, a3.value.value)(f, g)
  def xcoderiving4[Z, A1, A2, A3, A4](f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z,
                                      g: Z => (A1 \/ (A2 \/ (A3 \/ A4))))(
    implicit
    a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]],
    a4: Cached[Lazy[F[A4]]]
  ): F[Z] =
    xcoproduct4(a1.value.value, a2.value.value, a3.value.value, a4.value.value)(
      f,
      g
    )
}
object CoapplicativeCodivide {
  @inline def apply[F[_]](
    implicit i: CoapplicativeCodivide[F]
  ): CoapplicativeCodivide[F] = i
}
