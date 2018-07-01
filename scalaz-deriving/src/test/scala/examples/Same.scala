// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import java.lang.String

import scala.{ Boolean, Int, Long }

import scalaz._, Scalaz._

import simulacrum._

/**
 * Reimplementation of Equal, using Decidablez.
 */
@typeclass trait Same[A] {
  def same(a1: A, a2: A): Boolean

  final def different(a1: A, a2: A): Boolean = !same(a1, a2)
}
object Same {
  implicit val string: Same[String]   = (s1, s2) => s1 == s2
  implicit val long: Same[Long]       = (l1, l2) => l1 == l2
  implicit val boolean: Same[Boolean] = (b1, b2) => b1 == b2

  implicit val decidablez: Decidablez[Same] = new Decidablez[Same] {
    def productz[Z, G[_]: Traverse](f: Z =*> G): Same[Z] = { (z1, z2) =>
      f(z1, z2).all { case fa /~\ ((a1, a2)) => fa.same(a1, a2) }
    }

    def coproductz[Z](f: Z =+> Maybe): Same[Z] = { (z1, z2) =>
      f(z1, z2).map { case fa /~\ ((a1, a2)) => fa.same(a1, a2) }
        .getOrElse(false)
    }
  }

  implicit val int: Same[Int] = long.contramap(_.toLong)

}
