// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }
import scala.Predef.identity

import shapeless.{ Cached, Lazy }

// coproduct analogue of Productive
trait Coproductive[F[_]] extends Functor[F] with CoproductiveCodivide[F] {
  def coproduct1[Z, A1, A2](fb: F[A1])(f: A1 => Z): F[Z] = map(fb)(f)
  def coproduct2[Z, A1, A2](fb: => F[A1], fc: => F[A2])(f: A1 \/ A2 => Z): F[Z]
  // if this trait were inherited by Plus, it can be derived
  //map(plus[A1 \/ A2](map(fb)(-\/(_)), map(fc)(\/-(_))))(f)
  def coproduct3[Z, A1, A2, A3](fb: => F[A1], fc: => F[A2], fd: => F[A3])(
    f: A1 \/ (A2 \/ A3) => Z
  ): F[Z] = coproduct2(fb, either2(fc, fd))(f)
  def coproduct4[Z, A1, A2, A3, A4](fb: => F[A1],
                                    fc: => F[A2],
                                    fd: => F[A3],
                                    fe: => F[A4])(
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z
  ): F[Z] =
    coproduct2(fb, either2(fc, either2(fd, fe)))(f)
  // ... coproductX

  // equivalent of tupleX
  def either2[A1, A2](a1: => F[A1], a2: => F[A2]): F[A1 \/ A2] =
    coproduct2(a1, a2)(identity)
  // ... eitherX

  final def coapplying1[Z, A1](
    f: A1 => Z
  )(implicit a1: Cached[Lazy[F[A1]]]): F[Z] =
    coproduct1(a1.value.value)(f)
  final def coapplying2[Z, A1, A2](
    f: A1 \/ A2 => Z
  )(implicit a1: Cached[Lazy[F[A1]]], a2: Cached[Lazy[F[A2]]]): F[Z] =
    coproduct2(a1.value.value, a2.value.value)(f)
  final def coapplying3[Z, A1, A2, A3](
    f: A1 \/ (A2 \/ A3) => Z
  )(implicit a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]]): F[Z] =
    coproduct3(a1.value.value, a2.value.value, a3.value.value)(f)
  final def coapplying4[Z, A1, A2, A3, A4](
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z
  )(implicit a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]],
    a4: Cached[Lazy[F[A4]]]): F[Z] =
    coproduct4(a1.value.value, a2.value.value, a3.value.value, a4.value.value)(
      f
    )
  // ... coapplyingX

  override def xcoproduct2[Z, A1, A2](fa: => F[A1], fb: => F[A2])(
    f: (A1 \/ A2) => Z,
    g: Z => (A1 \/ A2)
  ): F[Z] = coproduct2(fa, fb)(f)
  override def xcoproduct3[Z, A1, A2, A3](fa: => F[A1],
                                          fb: => F[A2],
                                          fc: => F[A3])(
    f: (A1 \/ (A2 \/ A3)) => Z,
    g: Z => (A1 \/ (A2 \/ A3))
  ): F[Z] = coproduct3(fa, fb, fc)(f)
  override def xcoproduct4[Z, A1, A2, A3, A4](
    fa: => F[A1],
    fb: => F[A2],
    fc: => F[A3],
    fd: => F[A4]
  )(f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z,
    g: Z => (A1 \/ (A2 \/ (A3 \/ A4)))): F[Z] = coproduct4(fa, fb, fc, fd)(f)
}
object Coproductive {
  @inline def apply[F[_]](implicit i: Coproductive[F]): Coproductive[F] = i
}
