// Copyright: 2017 - 2020 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package scalaz
package iotatests

import scala._, Predef._
import iotaz._

import org.scalacheck._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop._
import org.scalacheck.ScalacheckShapeless._

object CopKTests extends Properties("CopKTests") {

  import TListK.::
  import TListK.Op._

  sealed abstract class One[A]
  object One {
    case class Value(a: String) extends One[Double]
  }

  sealed abstract class Two[A]
  object Two {
    case class Value(a: Int) extends Two[Int]
  }

  sealed abstract class Three[A]
  object Three {
    case class Value(a: Int) extends Three[Int]
  }

  type OneTwoThreeL = One :: Two :: Three :: TNilK
  type ThreeTwoOneL = Three :: Two :: One :: TNilK

  // these just need to compile
  CopK.InjectL[One, OneTwoThreeL]
  CopK.InjectL[CopKTests.One, OneTwoThreeL]
  CopK.InjectL[_root_.scalaz.iotatests.CopKTests.One, OneTwoThreeL]

  def checkInjectL[F[_], L <: TListK, A](
    gen: Gen[F[A]],
    inj: CopK.InjectL[F, L],
    index: Int
  ): Prop =
    forAll(gen)(v => inj.inj(v) ?= CopK.unsafeApply(index, v))

  property("inject One into OneTwoThreeL") =
    checkInjectL(arbitrary[One.Value], CopK.InjectL[One, OneTwoThreeL], 0)

  property("inject Two into OneTwoThreeL") =
    checkInjectL(arbitrary[Two.Value], CopK.InjectL[Two, OneTwoThreeL], 1)

  property("inject Three into OneTwoThreeL") =
    checkInjectL(arbitrary[Three.Value], CopK.InjectL[Three, OneTwoThreeL], 2)

  property("inject One into ThreeTwoOneL") =
    checkInjectL(arbitrary[One.Value], CopK.InjectL[One, ThreeTwoOneL], 2)

  property("inject Two into ThreeTwoOneL") =
    checkInjectL(arbitrary[Two.Value], CopK.InjectL[Two, ThreeTwoOneL], 1)

  property("inject Three into ThreeTwoOneL") =
    checkInjectL(arbitrary[Three.Value], CopK.InjectL[Three, ThreeTwoOneL], 0)

  property("inject Three into Reverse[ThreeTwoOneL]") = checkInjectL(
    arbitrary[Three.Value],
    CopK.InjectL[Three, Reverse[ThreeTwoOneL]],
    2
  )

  type First[A] = String
  type Last[A]  = A
  type Y[A]

  type Yuge =
    First :: // 20 rows of 15 = 300 filler items
    Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y :: Y ::
      Last ::
      TNilK

  property("inject First into Yuge") =
    checkInjectL(arbitrary[First[Int]], CopK.InjectL[First, Yuge], 0)

  property("inject Last into Yuge") =
    checkInjectL(arbitrary[Last[Int]], CopK.InjectL[Last, Yuge], 301)
}
