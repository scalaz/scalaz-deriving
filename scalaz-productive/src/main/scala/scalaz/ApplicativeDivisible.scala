// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ inline }

// an invariant parent of Applicative / Divisible
trait ApplicativeDivisible[F[_]] extends ApplyDivide[F] {
  def xproduct0[Z](f: => Z): F[Z]
  final def xderiving0[Z](z: Z): F[Z] = xproduct0(z)
}
object ApplicativeDivisible {
  @inline def apply[F[_]](
    implicit i: ApplicativeDivisible[F]
  ): ApplicativeDivisible[F] = i
}
