// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }
import scala.Predef.identity

import shapeless.{ Cached, Lazy }

// coproduct analogue of Divide
trait Codivide[F[_]] extends Contravariant[F] with CoapplicativeCodivide[F] {
  def codivide1[Z, A1, A2](a1: F[A1])(f: Z => A1): F[Z] = contramap(a1)(f)
  def codivide2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: Z => A1 \/ A2): F[Z]
  def codivide3[Z, A1, A2, A3](a1: => F[A1], a2: => F[A2], a3: => F[A3])(
    f: Z => A1 \/ (A2 \/ A3)
  ): F[Z] = {
    val a23: F[A2 \/ A3] = codivide2(a2, a3)(identity)
    codivide2(a1, a23)(f)
  }
  def codivide4[Z, A1, A2, A3, A4](a1: => F[A1],
                                   a2: => F[A2],
                                   a3: => F[A3],
                                   a4: => F[A4])(
    f: Z => A1 \/ (A2 \/ (A3 \/ A4))
  ): F[Z] = {
    val a34: F[A3 \/ A4]          = codivide2(a3, a4)(identity)
    val a234: F[A2 \/ (A3 \/ A4)] = codivide2(a2, a34)(identity)
    codivide2(a1, a234)(f)
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

  override final def xcoproduct2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(
    f: (A1 \/ A2) => Z,
    g: Z => (A1 \/ A2)
  ): F[Z] = codivide2(a1, a2)(g)
  override final def xcoproduct3[Z, A1, A2, A3](a1: => F[A1],
                                                a2: => F[A2],
                                                a3: => F[A3])(
    f: (A1 \/ (A2 \/ A3)) => Z,
    g: Z => (A1 \/ (A2 \/ A3))
  ): F[Z] = codivide3(a1, a2, a3)(g)
  override final def xcoproduct4[Z, A1, A2, A3, A4](
    a1: => F[A1],
    a2: => F[A2],
    a3: => F[A3],
    a4: => F[A4]
  )(f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z,
    g: Z => (A1 \/ (A2 \/ (A3 \/ A4)))): F[Z] = codivide4(a1, a2, a3, a4)(g)

}
object Codivide {
  @inline def apply[F[_]](implicit i: Codivide[F]): Codivide[F] = i
}
