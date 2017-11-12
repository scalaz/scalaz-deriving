// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }
import scala.Predef.identity

import shapeless.{ Cached, Lazy }

// copy/pasta of scalaz.Divide with by-name params and "Lazy" (Name)
// types (will hopefully replace Divide in scalaz7.3)
trait LazyDivide[F[_]] extends Contravariant[F] with ApplyDivide[F] {

  final def divide1[A1, Z](a1: F[A1])(f: Z => A1): F[Z] = contramap(a1)(f)

  def divide2[A1, A2, Z](a1: => F[A1], a2: => F[A2])(f: Z => (A1, A2)): F[Z]
  def divide3[A1, A2, A3, Z](a1: => F[A1], a2: => F[A2], a3: => F[A3])(
    f: Z => (A1, A2, A3)
  ): F[Z] =
    divide2(tuple2(a1, a2), a3) { z =>
      val t = f(z)
      ((t._1, t._2), t._3)
    }
  def divide4[A1, A2, A3, A4, Z](a1: => F[A1],
                                 a2: => F[A2],
                                 a3: => F[A3],
                                 a4: => F[A4])(
    f: Z => (A1, A2, A3, A4)
  ): F[Z] =
    divide2(tuple2(a1, a2), tuple2(a3, a4)) { z =>
      val t = f(z)
      ((t._1, t._2), (t._3, t._4))
    }

  def tuple2[A1, A2](a1: => F[A1], a2: => F[A2]): F[(A1, A2)] =
    divide2(a1, a2)(identity)

  final def dividing1[A1, Z](
    f: Z => A1
  )(implicit a1: Cached[Lazy[F[A1]]]): F[Z] =
    divide1(a1.value.value)(f)
  final def dividing2[A1, A2, Z](
    f: Z => (A1, A2)
  )(implicit a1: Cached[Lazy[F[A1]]], a2: Cached[Lazy[F[A2]]]): F[Z] =
    divide2(a1.value.value, a2.value.value)(f)
  final def dividing3[A1, A2, A3, Z](
    f: Z => (A1, A2, A3)
  )(implicit a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]]): F[Z] =
    divide3(a1.value.value, a2.value.value, a3.value.value)(f)
  final def dividing4[A1, A2, A3, A4, Z](
    f: Z => (A1, A2, A3, A4)
  )(implicit a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]],
    a4: Cached[Lazy[F[A4]]]): F[Z] =
    divide4(a1.value.value, a2.value.value, a3.value.value, a4.value.value)(f)

  // ApplyDivide impl
  override final def xproduct2[Z, A1, A2](
    a1: => F[A1],
    a2: => F[A2]
  )(f: (A1, A2) => Z, g: Z => (A1, A2)): F[Z] =
    divide2(a1, a2)(g)
  override final def xproduct3[Z, A1, A2, A3](a1: => F[A1],
                                              a2: => F[A2],
                                              a3: => F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z] = divide3(a1, a2, a3)(g)
  override final def xproduct4[Z, A1, A2, A3, A4](
    a1: => F[A1],
    a2: => F[A2],
    a3: => F[A3],
    a4: => F[A4]
  )(f: (A1, A2, A3, A4) => Z, g: Z => (A1, A2, A3, A4)): F[Z] =
    divide4(a1, a2, a3, a4)(g)

}
object LazyDivide {
  @inline def apply[F[_]](implicit i: LazyDivide[F]): LazyDivide[F] = i
}
