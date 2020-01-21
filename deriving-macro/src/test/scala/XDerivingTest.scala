// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package tests

import java.lang.String

import scala.{ Either, Right }

import org.scalatest.matchers.should.Matchers._
import shapeless.the
import testing.classes._
import testing.typeclasses._

import Cofoo.ops._
import org.scalatest.flatspec.AnyFlatSpec

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
