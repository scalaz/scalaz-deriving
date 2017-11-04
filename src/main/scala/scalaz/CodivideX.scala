// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

// TODO: list of coparams doesn't have the singular constraint on it... need a
//       better container.
/** Abstracts over arity of contravariant coproducts on a typeclass. */
sealed abstract class Coparam[Z, F[_]] {
  type T
  def apply(z: Z): Maybe[T]
  def tc: F[T]
}
object Coparam {
  def apply[Z, F[_], A](f: Z => Maybe[A])(FA: => F[A]) = new Coparam[Z, F] {
    type T = A
    override def apply(z: Z): Maybe[T] = f(z)
    override def tc: F[T]              = FA
  }
}

trait CodivideX[F[_]] extends Codivide[F] {
  def codivideX[C[_]: Foldable1, Z](params: C[Coparam[Z, F]]): F[Z]

  import Scalaz._
  override def contramap[A1, Z](a1: F[A1])(f: Z => A1): F[Z] =
    codivideX[NonEmptyList, Z] {
      NonEmptyList(Coparam[Z, F, A1](z => f(z).just)(a1))
    }
  override def codivide2[Z, A1, A2](fb: => F[A1],
                                    fc: => F[A2])(f: Z => A1 \/ A2): F[Z] =
    codivideX[NonEmptyList, Z] {
      NonEmptyList(
        Coparam[Z, F, A1] { z =>
          f(z) match {
            case -\/(b) => b.just
            case _      => Maybe.empty
          }
        }(fb),
        Coparam[Z, F, A2] { z =>
          f(z) match {
            case \/-(c) => c.just
            case _      => Maybe.empty
          }
        }(fc)
      )
    }
  override def codivide3[Z, A1, A2, A3](fb: => F[A1], fc: => F[A2], fd: F[A3])(
    f: Z => A1 \/ (A2 \/ A3)
  ): F[Z] = codivideX[NonEmptyList, Z] {
    NonEmptyList(
      Coparam[Z, F, A1] { z =>
        f(z) match {
          case -\/(b) => b.just
          case _      => Maybe.empty
        }
      }(fb),
      Coparam[Z, F, A2] { z =>
        f(z) match {
          case \/-(-\/(c)) => c.just
          case _           => Maybe.empty
        }
      }(fc),
      Coparam[Z, F, A3] { z =>
        f(z) match {
          case \/-(\/-(d)) => d.just
          case _           => Maybe.empty
        }
      }(fd)
    )
  }

  override def codivide4[Z, A1, A2, A3, A4](fb: => F[A1],
                                            fc: => F[A2],
                                            fd: => F[A3],
                                            fe: => F[A4])(
    f: Z => A1 \/ (A2 \/ (A3 \/ A4))
  ): F[Z] = codivideX[NonEmptyList, Z] {
    NonEmptyList(
      Coparam[Z, F, A1] { z =>
        f(z) match {
          case -\/(b) => b.just
          case _      => Maybe.empty
        }
      }(fb),
      Coparam[Z, F, A2] { z =>
        f(z) match {
          case \/-(-\/(c)) => c.just
          case _           => Maybe.empty
        }
      }(fc),
      Coparam[Z, F, A3] { z =>
        f(z) match {
          case \/-(\/-(-\/(d))) => d.just
          case _                => Maybe.empty
        }
      }(fd),
      Coparam[Z, F, A4] { z =>
        f(z) match {
          case \/-(\/-(\/-(e))) => e.just
          case _                => Maybe.empty
        }
      }(fe)
    )
  }
}
object CodivideX {
  @inline def apply[F[_]](implicit i: CodivideX[F]): CodivideX[F] = i
}
