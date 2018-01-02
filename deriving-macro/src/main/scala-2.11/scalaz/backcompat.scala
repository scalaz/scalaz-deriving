// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import scala.{ Either, Left, Right }

trait BackCompat {
  private[scalaz] implicit class EitherBackCompat[L, R](e: Either[L, R]) {
    def map[RR](f: R => RR): Either[L, RR] = e match {
      case Left(left)   => Left(left)
      case Right(right) => Right(f(right))
    }

    def flatMap[RR](f: R => Either[L, RR]): Either[L, RR] = e match {
      case Left(left)   => Left(left)
      case Right(right) => f(right)
    }
  }
}
