// Copyright: 2017 - 2019 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import java.lang.String

import scala.{ Boolean, Int, Long }

import scalaz._, Scalaz._

import simulacrum._

/**
 * Reimplementation of Equal, to test Decidable and ExtendedInvariantAlt.
 */
@typeclass trait Same[A] {
  def same(a1: A, a2: A): Boolean

  final def different(a1: A, a2: A): Boolean = !same(a1, a2)
}
object Same {
  implicit val string: Same[String]   = (s1, s2) => s1 == s2
  implicit val long: Same[Long]       = (l1, l2) => l1 == l2
  implicit val boolean: Same[Boolean] = (b1, b2) => b1 == b2

  implicit val decidable: Decidable[Same] = new Decidable[Same] {
    override def divide[A1, A2, Z](a1: Same[A1], a2: Same[A2])(
      f: Z => (A1, A2)
    ): Same[Z] = { (z1, z2) =>
      val (s1, s2) = f(z1)
      val (t1, t2) = f(z2)
      a1.same(s1, t1) && a2.same(s2, t2)
    }
    override def conquer[A]: Same[A] = (_, _) => true

    override def choose2[Z, A1, A2](a1: =>Same[A1], a2: =>Same[A2])(
      f: Z => A1 \/ A2
    ): Same[Z] = { (z1, z2) =>
      (f(z1), f(z2)) match {
        case (-\/(s), -\/(t)) => a1.same(s, t)
        case (\/-(s), \/-(t)) => a2.same(s, t)
        case _                => false
      }
    }
  }

  implicit val deriving: Deriving[Same] = ExtendedInvariantAlt(decidable)

  implicit val int: Same[Int] = long.contramap(_.toLong)

}
