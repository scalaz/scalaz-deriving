// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package tests

import java.lang.String

import scala.{ Either, Right }

import org.scalatest._
import org.scalatest.Matchers._
import play.api.libs.json
import shapeless.{ the, Generic, LabelledGeneric }
import testing.classes._
import testing.typeclasses._

import Cofoo.ops._

class DerivingTest extends FlatSpec {

  "@deriving" should "support case classes" in {
    the[Cofoo[Foo]] shouldBe Foo.`testing.typeclasses.Cofoo`
    the[Cofoo[Foo]] should not equal null
  }

  it should "support typeclasses in the same compilation unit" in {
    the[Wibble[Foo]] shouldBe Foo.`testing.classes.Wibble`
    the[Wibble[Foo]] should not equal null
  }

  it should "support case classes with a companion" in {
    the[Cofoo[Bar]] shouldBe Bar.`testing.typeclasses.Cofoo`
    the[Cofoo[Bar]] should not equal null
  }

  it should "support case classes with type parameters" in {
    the[json.Format[Gaz[String]]] should not equal null
    Gaz.`play.api.libs.json.Format`[String] should not equal null
  }

  // it should "support HKT typeclasses" in {
  //   // also doubles as a test of FQN handling

  //   the[testing.typeclasses.a.Cobaz[Gaz]] should not equal null
  //   Gaz.`testing.typeclasses.a.Cobaz` should not equal null

  //   the[testing.typeclasses.b.Cobaz[Gaz]] should not equal null
  //   Gaz.`testing.typeclasses.b.Cobaz` should not equal null
  // }

  it should "support sealed traits" in {
    the[Cofoo[Baz]] shouldBe Baz.`testing.typeclasses.Cofoo`
    the[Cofoo[Baz]] should not equal null
    the[Cobar[Baz]] shouldBe Baz.`testing.typeclasses.Cobar`
    the[Cobar[Baz]] should not equal null
  }

  it should "special case AnyVal" in {
    the[Cofoo[Anyz]] should not equal null

    the[Cofoo[Anyz]] shouldBe Anyz.`testing.typeclasses.Cofoo`

    Anyz("wibble").toFoo shouldBe "exercised the xmap codepath"

    new Anyzz("wobble").toFoo shouldBe "exercised the xmap codepath"
  }

  it should "special case AnyVal with type parameters" in {
    val e: Either[String, String] = Right("hello")
    new Valuezz(e).toFoo shouldBe "exercised the xmap codepath"
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
    the[Cobar[Foo]] shouldBe Foo.`testing.typeclasses.Cobar`
    the[Cobar[Foo]] should not equal null
  }

  it should "support the .Aux pattern on regular classes" in {
    Bar.`shapeless.Generic` should not equal null
    val g = Generic[Bar]
    Bar.`shapeless.Generic` should be theSameInstanceAs (g)
    g shouldBe an[Generic.Aux[Bar, g.Repr]]

    Bar.`shapeless.LabelledGeneric` should not equal null
    val lg = LabelledGeneric[Bar]
    Bar.`shapeless.LabelledGeneric` should be theSameInstanceAs (lg)
    lg shouldBe an[LabelledGeneric.Aux[Bar, lg.Repr]]
  }

  it should "support the .Aux pattern on parameterised classes" in {
    Gaz.`shapeless.Generic`[String] should not equal null
    val g = Generic[Gaz[String]]
    g shouldBe an[Generic.Aux[Gaz[String], g.Repr]]

    Gaz.`shapeless.LabelledGeneric`[String] should not equal null
    val lg = LabelledGeneric[Gaz[String]]
    lg shouldBe an[LabelledGeneric.Aux[Gaz[String], lg.Repr]]
  }

  it should "provide position information on failure" ignore {
    // https://github.com/milessabin/shapeless/issues/756
    // https://github.com/scalatest/scalatest/issues/1193
    fail("see below, must be manual")
  }
}

// @scalaz.deriving(Cobar)
// class ElZilcho(s: String)

// AnyVal cannot be defined in a test
// should fail with "value xmap is not a member of ..."
//@scalaz.deriving(Cobar)
//class Bad(val s: String) extends scala.AnyVal
