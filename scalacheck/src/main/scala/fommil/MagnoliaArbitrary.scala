// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package fommil

import magnolia._, mercator._
import org.scalacheck._

import scalaz._, Scalaz._
import scalaz.scalacheck.ScalaCheckBinding._

// although it is possible to use the scalaz.Deriving API to define an Arbitrary
// it will never satisfy the Alt laws and will always be at risk of causing
// typeclass decoherence with the Applicative from ScalaCheckBinding, so it's
// best to use Magnolia.
object MagnoliaArbitrary {
  type Typeclass[A] = Arbitrary[A]

  implicit val monadicGen: Monadic[Gen] = new Monadic[Gen] {
    def flatMap[A, B](fa: Gen[A], f: A => Gen[B]): Gen[B] = fa.flatMap(f)
    def map[A, B](fa: Gen[A], f: A => B): Gen[B]          = fa.map(f)
    def point[A](a: A): Gen[A]                            = a.point[Gen]
  }

  def combine[A](ctx: CaseClass[Arbitrary, A]): Arbitrary[A] = Arbitrary {
    ctx.constructMonadic(p => Gen.lzy(p.typeclass.arbitrary))
  }

  def dispatch[A](ctx: SealedTrait[Arbitrary, A]): Arbitrary[A] = Arbitrary {
    Gen.frequency(
      ctx.subtypes.map(s => 1 -> Gen.lzy(s.typeclass.arbitrary)): _*
    )
  }

  def gen[A]: Arbitrary[A] = macro Magnolia.gen[A]
}
