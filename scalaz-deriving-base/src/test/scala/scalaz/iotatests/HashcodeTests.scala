/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

package scalaz
package iotatests

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.*
import org.scalacheck.ScalacheckShapeless.*
import scala.*
import scala.Predef.*
import scalaz.iotaz.*

object HashcodeTests extends Properties("HashcodeTests") {

  property("keep equals - hashcode contract for CopK") = {
    import TListK.::
    type CC[A] = CopK[List :: TNilK, A]
    val I = CopK.Inject[List, CC]
    forAll(arbitrary[Map[List[Int], String]].suchThat(_.size > 0)) { map =>
      val copMap = map.map { case (k, v) => (I(k), v) }
      val key = copMap.keys.head
      val keyCopy = I(I.prj(key).get)

      copMap.get(keyCopy) ?= copMap.get(key)
    }
  }

}
