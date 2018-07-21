// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import java.lang.String

import org.scalacheck.{ Arbitrary, Gen }
import scalaz._, Scalaz._
import scalaz.scalacheck.ScalaCheckBinding._

object DerivedArbitrary {
  // we can't have an Alt because it breaks derived combinator RT... the first
  // element of a coproduct is always weighted more heavily. We could, however,
  // have an Applicativez but that would break typeclass coherence with the
  // instance in scalaz.scalacheck.ScalaCheckBinding
  implicit val deriving_arbitrary: Deriving[Arbitrary] =
    new Deriving[Arbitrary] {

      private val pick = λ[NameF ~> Gen](a => a.value.arbitrary)

      def xproductz[Z, A <: TList, TC <: TList, L <: TList](
        tcs: Prod[TC],
        labels: Prod[L],
        @unused name: String
      )(
        f: Prod[A] => Z,
        g: Z => Prod[A]
      )(
        implicit
        ev1: NameF ƒ A ↦ TC,
        ev2: Label ƒ A ↦ L
      ): Arbitrary[Z] =
        Arbitrary(tcs.traverse(pick).map(f))

      type SGen[a] = IStream[Gen[a]]
      private val always = λ[NameF ~> SGen](
        a => IStream.Lazy(Gen.lzy(a.value.arbitrary))
      )

      def xcoproductz[Z, A <: TList, TC <: TList, L <: TList](
        tcs: Prod[TC],
        labels: Prod[L],
        @unused name: String
      )(
        f: Cop[A] => Z,
        g: Z => Cop[A]
      )(
        implicit
        ev1: NameF ƒ A ↦ TC,
        ev2: Label ƒ A ↦ L
      ): Arbitrary[Z] = Arbitrary {
        Gen.frequency(
          tcs
            .coptraverse(always)
            .toList
            .map(g => (1, g.map(f))): _*
        )
      }
    }

}
