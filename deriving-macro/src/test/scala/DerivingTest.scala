// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package tests

import java.lang.String
import org.scalatest.matchers.should.Matchers._
import play.api.libs.json
import play.api.libs.json.{ JsArray, JsString }
import scala.collection.immutable.List
import shapeless.the
import testing.classes._
import testing.typeclasses._
import Cofoo.ops._
import org.scalatest.flatspec.AnyFlatSpec

class DerivingTest extends AnyFlatSpec {
  // scalafix:off DisableSyntax.keywords.null

  "@deriving" should "support case classes" in {
    the[Cofoo[Foo]].shouldBe(Foo._deriving_cofoo)
    (the[Cofoo[Foo]] should not).equal(null)
  }

  it should "support typeclasses in the same compilation unit" in {
    the[Wibble[Foo]].shouldBe(Foo._deriving_wibble)
    (the[Wibble[Foo]] should not).equal(null)
  }

  it should "support case classes with a companion" in {
    the[Cofoo[Bar]].shouldBe(Bar._deriving_cofoo)
    (the[Cofoo[Bar]] should not).equal(null)
  }

  it should "support case classes with type parameters" in {
    (the[json.Format[Gaz[String]]] should not).equal(null)
    (Gaz._deriving_json_format[String] should not).equal(null)
  }

  it should "support classes with type parameters" in {
    (the[Cofoo[Waz[String]]] should not).equal(null)
    (Waz._deriving_cofoo[String] should not).equal(null)
  }

  // it should "support HKT typeclasses" in {
  //   // also doubles as a test of FQN handling

  //   the[testing.typeclasses.a.Cobaz[Gaz]] should not equal null
  //   Gaz.`testing.typeclasses.a.Cobaz` should not equal null

  //   the[testing.typeclasses.b.Cobaz[Gaz]] should not equal null
  //   Gaz.`testing.typeclasses.b.Cobaz` should not equal null
  // }

  it should "support sealed traits" in {
    the[Cofoo[Baz]].shouldBe(Baz._deriving_cofoo)
    (the[Cofoo[Baz]] should not).equal(null)
    the[Cobar[Baz]].shouldBe(Baz._deriving_b)
    (the[Cobar[Baz]] should not).equal(null)
  }

  it should "not special case AnyVal" in {
    (the[Cofoo[Anyz]] should not).equal(null)

    the[Cofoo[Anyz]].shouldBe(Anyz._deriving_cofoo)

    Anyz("wibble").toFoo.shouldBe("this is the default gen codepath")
  }

  it should "support baked-in rules" in {
    the[json.Format[Foo]].shouldBe(Foo._deriving_json_format)
    (the[json.Format[Foo]] should not).equal(null)
  }

  it should "support user-provided rules" in {
    the[Cobar[Foo]].shouldBe(Foo._deriving_b)
    (the[Cobar[Foo]] should not).equal(null)
  }

  it should "support val forwarders" in {
    (D._deriving_d_valforwarder should not).equal(null)
  }

  it should "special case @newtype" in {
    import newtypes._
    val res = scala.Predef.implicitly[json.Format[Spam]].writes(Spam("hello"))
    res.shouldBe(JsString("hello"))
  }

  it should "special case @newsubtype" in {
    import newtypes._
    val res = scala.Predef.implicitly[json.Format[Eggs]].writes(Eggs("hello"))
    res.shouldBe(JsString("hello"))
  }

  it should "special case @newtype with type params" in {
    import newtypes._
    val res = scala.Predef
      .implicitly[json.Format[Spammer[String]]]
      .writes(Spammer(List("hello")))
    res.shouldBe(JsArray(List(JsString("hello"))))
  }

  it should "special case @newsubtype with type params" in {
    import newtypes._
    val res = scala.Predef
      .implicitly[json.Format[EggHead[String]]]
      .writes(EggHead(List("hello")))
    res.shouldBe(JsArray(List(JsString("hello"))))
  }
}
