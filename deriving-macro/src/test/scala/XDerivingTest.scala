// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package tests

import java.lang.String

import scala.{ Either, Right }

import org.scalatest._
import org.scalatest.Matchers._
import shapeless.the
import testing.classes._
import testing.typeclasses._

import Cofoo.ops._

class XDerivingTest extends FlatSpec {

  "@xderiving" should "support AnyVal" in {
    (the[Cofoo[Anyx]] should not).equal(null)

    the[Cofoo[Anyx]].shouldBe(Anyx.`testing.typeclasses.Cofoo`)

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
    (the[Cobar[Van]] should not).equal(null)
  }

  it should "fail to derive AnyVal that is not invariant" ignore {
    fail("see below, must be manual")
  }

}

// AnyVal cannot be defined in a test
// should fail with "value xmap is not a member of ..."
//@scalaz.deriving(Cobar)
//class Bad(val s: String) extends scala.AnyVal
