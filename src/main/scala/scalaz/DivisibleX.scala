// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

/** Implementation of Divisible in terms of a single, generic, method. */
trait DivisibleX[F[_]] extends LazyDivisible[F] {
  def divideX[Z](f: Z => ProductX[F]): F[Z]

  override def conquer[Z]: F[Z] = divideX[Z] { _ =>
    ProductX(IList.empty)
  }
  override def contramap[A1, Z](a1: F[A1])(f: Z => A1): F[Z] =
    divideX[Z] { z =>
      ProductX(IList(ParamX(f(z), a1)))
    }

  override def divide2[A1, A2, Z](a1: => F[A1],
                                  a2: => F[A2])(f: Z => (A1, A2)): F[Z] = {
    val mf = Memo.immutableListMapMemo(f)
    divideX[Z] { z =>
      ProductX(
        IList(
          ParamX(mf(z)._1, a1),
          ParamX(mf(z)._2, a2)
        )
      )
    }
  }
  override def divide3[A1, A2, A3, Z](a1: => F[A1], a2: => F[A2], a3: => F[A3])(
    f: Z => (A1, A2, A3)
  ): F[Z] = {
    val mf = Memo.immutableListMapMemo(f)
    divideX[Z] { z =>
      ProductX(
        IList(
          ParamX(mf(z)._1, a1),
          ParamX(mf(z)._2, a2),
          ParamX(mf(z)._3, a3)
        )
      )
    }
  }
  override def divide4[A1, A2, A3, A4, Z](a1: => F[A1],
                                          a2: => F[A2],
                                          a3: => F[A3],
                                          a4: => F[A4])(
    f: Z => (A1, A2, A3, A4)
  ): F[Z] = {
    val mf = Memo.immutableListMapMemo(f)
    divideX[Z] { z =>
      ProductX(
        IList(
          ParamX(mf(z)._1, a1),
          ParamX(mf(z)._2, a2),
          ParamX(mf(z)._3, a3),
          ParamX(mf(z)._4, a4)
        )
      )
    }
  }
}
object DivisibleX {
  @inline def apply[F[_]](implicit i: DivisibleX[F]): DivisibleX[F] = i
}
