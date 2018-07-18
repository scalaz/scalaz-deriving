// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import org.scalacheck.{ Arbitrary, Gen }
import scalaz._

object DerivedArbitrary {
  // hide to avoid breaking typeclass coherence
  private val _alt_arbitrary: Alt[Arbitrary] = new Alt[Arbitrary] {
    override def point[A](a: =>A): Arbitrary[A] = Arbitrary(a)

    override def apply2[A1, A2, Z](a1: =>Arbitrary[A1], a2: =>Arbitrary[A2])(
      f: (A1, A2) => Z
    ): Arbitrary[Z] = Arbitrary {
      for {
        a <- a1.arbitrary
        b <- a2.arbitrary
      } yield f(a, b)
    }

    override def altly2[Z, A1, A2](a1: =>Arbitrary[A1], a2: =>Arbitrary[A2])(
      f: A1 \/ A2 => Z
    ): Arbitrary[Z] = Arbitrary {
      // strong bias to pick the left when nested...
      Gen
        .oneOf(
          Gen.lzy(a1.arbitrary.map(-\/(_))),
          Gen.lzy(a2.arbitrary.map(\/-(_)))
        )
        .map(f)
    }

    // swaparoo the derived combinators
    override def ap[A, B](
      fa: =>Arbitrary[A]
    )(f: =>Arbitrary[A => B]): Arbitrary[B] =
      apply2(fa, f)((a, abc) => abc(a))
    override def alt[A](a1: =>Arbitrary[A], a2: =>Arbitrary[A]): Arbitrary[A] =
      altly2(a1, a2) {
        case -\/(a) => a
        case \/-(a) => a
      }
  }

  implicit val _arbitrary_deriving: Deriving[Arbitrary] = ExtendedInvariantAlt(
    _alt_arbitrary
  )

}
