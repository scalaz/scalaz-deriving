// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }
import scala.Predef.identity

import shapeless.{ Cached, Lazy }

// coproduct analogue of Divide
trait Codivide[F[_]] extends Contravariant[F] with CoproductiveCodivide[F] {
  def codivide1[Z, A1, A2](fb: F[A1])(f: Z => A1): F[Z] = contramap(fb)(f)
  def codivide2[Z, A1, A2](fb: => F[A1], fc: => F[A2])(f: Z => A1 \/ A2): F[Z]
  def codivide3[Z, A1, A2, A3](fb: => F[A1], fc: => F[A2], fd: F[A3])(
    f: Z => A1 \/ (A2 \/ A3)
  ): F[Z] = {
    val fcd: F[A2 \/ A3] = codivide2(fc, fd)(identity)
    codivide2(fb, fcd)(f)
  }
  def codivide4[Z, A1, A2, A3, A4](fb: => F[A1],
                                   fc: => F[A2],
                                   fd: => F[A3],
                                   fe: => F[A4])(
    f: Z => A1 \/ (A2 \/ (A3 \/ A4))
  ): F[Z] = {
    val fde: F[A3 \/ A4]          = codivide2(fd, fe)(identity)
    val fcde: F[A2 \/ (A3 \/ A4)] = codivide2(fc, fde)(identity)
    codivide2(fb, fcde)(f)
  }
  // ... codivideX

  final def codividing2[Z, A1, A2](
    f: Z => A1 \/ A2
  )(implicit fa1: Cached[Lazy[F[A1]]], fa2: Cached[Lazy[F[A2]]]): F[Z] =
    codivide2(fa1.value.value, fa2.value.value)(f)
  final def codividing3[Z, A1, A2, A3](
    f: Z => A1 \/ (A2 \/ A3)
  )(implicit fa1: Cached[Lazy[F[A1]]],
    fa2: Cached[Lazy[F[A2]]],
    fa3: Cached[Lazy[F[A3]]]): F[Z] =
    codivide3(fa1.value.value, fa2.value.value, fa3.value.value)(f)
  final def codividing4[Z, A1, A2, A3, A4](
    f: Z => A1 \/ (A2 \/ (A3 \/ A4))
  )(implicit fa1: Cached[Lazy[F[A1]]],
    fa2: Cached[Lazy[F[A2]]],
    fa3: Cached[Lazy[F[A3]]],
    fa4: Cached[Lazy[F[A4]]]): F[Z] =
    codivide4(fa1.value.value,
              fa2.value.value,
              fa3.value.value,
              fa4.value.value)(f)
  // ... codividingX

  override final def xcoproduct2[Z, A1, A2](fa: => F[A1], fb: => F[A2])(
    f: (A1 \/ A2) => Z,
    g: Z => (A1 \/ A2)
  ): F[Z] = codivide2(fa, fb)(g)
  override final def xcoproduct3[Z, A1, A2, A3](fa: => F[A1],
                                                fb: => F[A2],
                                                fc: => F[A3])(
    f: (A1 \/ (A2 \/ A3)) => Z,
    g: Z => (A1 \/ (A2 \/ A3))
  ): F[Z] = codivide3(fa, fb, fc)(g)
  override final def xcoproduct4[Z, A1, A2, A3, A4](
    fa: => F[A1],
    fb: => F[A2],
    fc: => F[A3],
    fd: => F[A4]
  )(f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z,
    g: Z => (A1 \/ (A2 \/ (A3 \/ A4)))): F[Z] = codivide4(fa, fb, fc, fd)(g)

}
object Codivide {
  @inline def apply[F[_]](implicit i: Codivide[F]): Codivide[F] = i
}
