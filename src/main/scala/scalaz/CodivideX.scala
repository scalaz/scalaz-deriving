// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.inline

trait CodivideX[F[_]] extends Codivide[F] {
  def codivideX[Z](c: Z => CoproductX[F]): F[Z]

  override def contramap[A1, Z](a1: F[A1])(f: Z => A1): F[Z] =
    codivideX[Z] { z =>
      CoproductX(0, ParamX(f(z), a1))
    }
  override def codivide2[Z, A1, A2](fb: => F[A1],
                                    fc: => F[A2])(f: Z => A1 \/ A2): F[Z] =
    codivideX[Z] { z =>
      f(z) match {
        case -\/(b) => CoproductX(0, ParamX(b, fb))
        case \/-(c) => CoproductX(1, ParamX(c, fc))
      }
    }
  override def codivide3[Z, A1, A2, A3](fb: => F[A1], fc: => F[A2], fd: F[A3])(
    f: Z => A1 \/ (A2 \/ A3)
  ): F[Z] = codivideX[Z] { z =>
    f(z) match {
      case -\/(b)      => CoproductX(0, ParamX(b, fb))
      case \/-(-\/(c)) => CoproductX(1, ParamX(c, fc))
      case \/-(\/-(d)) => CoproductX(2, ParamX(d, fd))
    }
  }

  override def codivide4[Z, A1, A2, A3, A4](fb: => F[A1],
                                            fc: => F[A2],
                                            fd: => F[A3],
                                            fe: => F[A4])(
    f: Z => A1 \/ (A2 \/ (A3 \/ A4))
  ): F[Z] = codivideX[Z] { z =>
    f(z) match {
      case -\/(b)           => CoproductX(0, ParamX(b, fb))
      case \/-(-\/(c))      => CoproductX(1, ParamX(c, fc))
      case \/-(\/-(-\/(d))) => CoproductX(2, ParamX(d, fd))
      case \/-(\/-(\/-(e))) => CoproductX(3, ParamX(e, fe))
    }
  }
}
object CodivideX {
  @inline def apply[F[_]](implicit i: CodivideX[F]): CodivideX[F] = i
}
