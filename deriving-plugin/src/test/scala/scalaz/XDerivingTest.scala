/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package scalaz

import java.lang.String
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scala.Predef.implicitly
import testing.*
import testing.typeclasses.*

class XDerivingTest extends AnyFlatSpec {

  "@xderiving" should "support AnyVal" in {
    implicitly[Cofoo[Anyx]].shouldBe(Anyx._deriving_cofoo)
  }

  it should "support AnyVal with type parameters" in {
    implicit val string: Cofoo[String] = null
    Valuezz._deriving_cofoo.shouldBe(null)
  }

  it should "support single parameter classes" in {
    implicitly[Cofoo[Van]].shouldBe(Van._deriving_cofoo)
    implicitly[Cobar[Van]].shouldBe(Van._deriving_b)
  }
}
