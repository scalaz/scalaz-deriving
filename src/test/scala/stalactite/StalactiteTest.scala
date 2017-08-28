// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalactite.tests

import java.lang.String

import org.scalatest._
import org.scalatest.Matchers._
import play.api.libs.json
import shapeless.the
import stalactite.examples._
import stalactite.typeclasses._

import Cofoo.ops._

class StalactiteTest extends FlatSpec {

  "@deriving" should "support case classes" in {
    the[Cofoo[Foo]] shouldBe Foo.`stalactite.typeclasses.Cofoo`
    the[Cofoo[Foo]] should not equal null
  }

  it should "support typeclasses in the same compilation unit" in {
    the[Wibble[Foo]] shouldBe Foo.`stalactite.examples.Wibble`
    the[Wibble[Foo]] should not equal null
  }

  it should "support case classes with a companion" in {
    the[Cofoo[Bar]] shouldBe Bar.`stalactite.typeclasses.Cofoo`
    the[Cofoo[Bar]] should not equal null
  }

  it should "support case classes with type parameters" in {
    the[json.Format[Gaz[String]]] should not equal null
    Gaz.`play.api.libs.json.Format`[String] should not equal null
  }

  // https://github.com/fommil/stalactite/issues/3
  // it should "support HKT typeclasses" in {
  //   // also doubles as a test of FQN handling

  //   the[stalactite.typeclasses.a.Cobaz[Gaz]] should not equal null
  //   Gaz.`stalactite.typeclasses.a.Cobaz` should not equal null

  //   the[stalactite.typeclasses.b.Cobaz[Gaz]] should not equal null
  //   Gaz.`stalactite.typeclasses.b.Cobaz` should not equal null
  // }

  it should "support sealed traits" in {
    the[Cofoo[Baz]] shouldBe Baz.`stalactite.typeclasses.Cofoo`
    the[Cofoo[Baz]] should not equal null
    the[Cobar[Baz]] shouldBe Baz.`stalactite.typeclasses.Cobar`
    the[Cobar[Baz]] should not equal null
  }

  it should "special case AnyVal" in {
    the[Cofoo[Anyz]] should not equal null

    the[Cofoo[Anyz]] shouldBe Anyz.`stalactite.typeclasses.Cofoo`

    Anyz("wibble").toFoo shouldBe "exercised the xmap codepath"

    new Anyzz("wobble").toFoo shouldBe "exercised the xmap codepath"
  }

  it should "support AnyVal for typeclasses with an InvariantFunctor" in {
    the[Cobar[Anyz]] should not equal null

    the[Cobar[Anyzz]] should not equal null
  }

  it should "fail to derive AnyVal that is not invariant" ignore {
    fail("see below, must be manual")
  }

  it should "support baked-in rules" in {
    the[json.Format[Foo]] shouldBe Foo.`play.api.libs.json.Format`
    the[json.Format[Foo]] should not equal null
  }

  it should "support user-provided rules" in {
    the[Cobar[Foo]] shouldBe Foo.`stalactite.typeclasses.Cobar`
    the[Cobar[Foo]] should not equal null
  }

  it should "provide position information on failure" ignore {
    // https://github.com/milessabin/shapeless/issues/756
    // https://github.com/scalatest/scalatest/issues/1193
    fail("see below, must be manual")
  }
}

// @stalactite.deriving(Cobar)
// class ElZilcho(s: String)

// AnyVal cannot be defined in a test
// should fail with "value xmap is not a member of ..."
//@stalactite.deriving(Cobar)
//class Bad(val s: String) extends scala.AnyVal
