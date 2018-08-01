// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package fommil

import magnolia._
import org.scalacheck._

import scalaz._, Scalaz._
import scalaz.scalacheck.ScalaCheckBinding._

// although it is possible to use the scalaz.Deriving API to define an Arbitrary
// it will never satisfy the Alt laws and will always be at risk of causing
// typeclass decoherence with the Applicative from ScalaCheckBinding, so it's
// best to use Magnolia.
object MagnoliaArbitrary {
  type Typeclass[A] = Arbitrary[A]

  def combine[A](ctx: CaseClass[Arbitrary, A]): Arbitrary[A] = Arbitrary {
    ctx.parameters.toList
      .traverse(p => Gen.lzy(p.typeclass.arbitrary))
      .map(ctx.rawConstruct)
  }

  def dispatch[A](ctx: SealedTrait[Arbitrary, A]): Arbitrary[A] = Arbitrary {
    Gen.frequency(
      ctx.subtypes.map(s => 1 -> Gen.lzy(s.typeclass.arbitrary)): _*
    )
  }

  def gen[A]: Arbitrary[A] = macro Magnolia.gen[A]
}
