// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

import shapeless.{ Cached, Lazy }

// a variant of Applicative introducing the new hierarchy and swapping the base
// implementation. May be merged into Applicative in scalaz7.3
trait Productive[F[_]] extends Applicative[F] with ApplicativeDivisible[F] {
  def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] =
    apply2(fa, f)((a, abc) => abc(a))
  override final def apply2[A, B, C](fa: => F[A],
                                     fb: => F[B])(f: (A, B) => C): F[C] =
    product2(fa, fb)(f)
  // product2 is a workaround because we can't make apply2 abstract
  // https://contributors.scala-lang.org/t/ability-to-make-a-parents-concrete-method-abstract/1255
  def product2[Z, A1, A2](fa: => F[A1], fb: => F[A2])(f: (A1, A2) => Z): F[Z]
  // should we create productX aliases?

  final def applying1[Z, A1](f: A1 => Z)(
    implicit a1: Cached[Lazy[F[A1]]]
  ): F[Z] = map(a1.value.value)(f)
  final def applying2[Z, A1, A2](
    f: (A1, A2) => Z
  )(implicit a1: Cached[Lazy[F[A1]]], a2: Cached[Lazy[F[A2]]]): F[Z] =
    apply2(a1.value.value, a2.value.value)(f)
  final def applying3[Z, A1, A2, A3](
    f: (A1, A2, A3) => Z
  )(implicit a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]]): F[Z] =
    apply3(a1.value.value, a2.value.value, a3.value.value)(f)
  final def applying4[Z, A1, A2, A3, A4](
    f: (A1, A2, A3, A4) => Z
  )(implicit a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]],
    a4: Cached[Lazy[F[A4]]]): F[Z] =
    apply4(a1.value.value, a2.value.value, a3.value.value, a4.value.value)(f)
  // ... applyingX

  // ApplicativeDivisible impl
  final override def xproduct0[Z](z: => Z): F[Z] = pure(z)
  override def xproduct2[Z, A1, A2](
    fa: => F[A1],
    fb: => F[A2]
  )(f: (A1, A2) => Z, g: Z => (A1, A2)): F[Z] =
    apply2(fa, fb)(f)
  override def xproduct3[Z, A1, A2, A3](fa: => F[A1],
                                        fb: => F[A2],
                                        fc: => F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z] = apply3(fa, fb, fc)(f)
  override def xproduct4[Z, A1, A2, A3, A4](
    fa: => F[A1],
    fb: => F[A2],
    fc: => F[A3],
    fd: => F[A4]
  )(f: (A1, A2, A3, A4) => Z, g: Z => (A1, A2, A3, A4)): F[Z] =
    apply4(fa, fb, fc, fd)(f)

}
object Productive {
  @inline def apply[F[_]](implicit i: Productive[F]): Productive[F] = i
}
