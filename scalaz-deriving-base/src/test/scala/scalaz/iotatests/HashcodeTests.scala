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

object HashcodeTests extends Properties("HashcodeTests") {

  property("keep equals - hashcode contract for CopK") = {
    import TListK.::
    type CC[A] = CopK[List :: TNilK, A]
    val I = CopK.Inject[List, CC]
    forAll(arbitrary[Map[List[Int], String]].suchThat(_.size > 0)) { map =>
      val copMap  = map.map { case (k, v) => (I(k), v) }
      val key     = copMap.keys.head
      val keyCopy = I(I.prj(key).get)

      copMap.get(keyCopy) ?= copMap.get(key)
    }
  }

}
