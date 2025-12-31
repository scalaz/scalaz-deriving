/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package tests

import java.lang.String
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import scala.Either
import scala.Right
import shapeless.the
import testing.classes.*
import testing.typeclasses.*
import testing.typeclasses.Cofoo.ops.*

class XDerivingTest extends AnyFlatSpec {
  // scalafix:off DisableSyntax.keywords.null

  "@xderiving" should "support AnyVal" in {
    (the[Cofoo[Anyx]] should not).equal(null)

    the[Cofoo[Anyx]].shouldBe(Anyx._deriving_cofoo)

    Anyx("wibble").toFoo.shouldBe("exercised the xmap codepath")

    new Anyzz("wobble").toFoo.shouldBe("exercised the xmap codepath")
  }

  it should "support AnyVal with type parameters" in {
    val e: Either[String, String] = Right("hello")
    new Valuezz(e).toFoo.shouldBe("exercised the xmap codepath")
  }

  it should "support AnyVal for typeclasses with an InvariantFunctor" in {
    (the[Cobar[Anyx]] should not).equal(null)

    (the[Cobar[Anyzz]] should not).equal(null)
  }

  it should "support single parameter classes" in {
    (the[Cofoo[Van]] should not).equal(null)
    (the[Cobar[Van]] should not).equal(null)
  }

  it should "support orphan instances" in {
    (the[OrphanCobar[Van]] should not).equal(null)
  }

}
