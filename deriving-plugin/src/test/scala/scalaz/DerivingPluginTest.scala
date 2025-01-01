// Copyright: 2017 - 2025 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String
import scala.Predef.implicitly

import org.scalatest.matchers.should.Matchers._

import testing._
import testing.typeclasses._
import org.scalatest.flatspec.AnyFlatSpec

class DerivingPluginTest extends AnyFlatSpec {

  "@deriving" should "support case classes" in {
    implicitly[Cofoo[Foo]].shouldBe(Foo._deriving_cofoo)
  }

  it should "support typeclasses in implicitly same compilation unit" in {
    implicitly[Wibble[Foo]].shouldBe(Foo._deriving_wibble)
  }

  it should "support case classes with a companion" in {
    implicitly[Cofoo[Bar]].shouldBe(Bar._deriving_cofoo)
  }

  it should "support case classes with type parameters" in {
    implicitly[json.Format[Gaz[String]]]
      .shouldBe(Gaz._deriving_json_format[String])
  }

  it should "support sealed traits" in {
    implicitly[Cofoo[Baz]].shouldBe(Baz._deriving_cofoo)
    implicitly[Cobar[Baz]].shouldBe(Baz._deriving_b)
  }

  it should "support baked-in rules" in {
    implicitly[json.Format[Foo]].shouldBe(Foo._deriving_json_format)
  }

  it should "support user-provided rules" in {
    implicitly[Cobar[Foo]].shouldBe(Foo._deriving_b)
  }

  it should "support val forwarders" in {
    D._deriving_d_valforwarder.shouldBe(implicitly[Cofoo[D]])
  }

  it should "not trigger SI-3664" in {
    val _: String => Si3664 = Si3664

    Si3664.toString.shouldBe("Si3664")
  }

  it should "support nested types" in {
    implicitly[Cofoo[Duped]].shouldBe(Duped._deriving_cofoo)

    implicitly[Cofoo[Duped.Souped]].shouldBe(Duped.Souped._deriving_cofoo)
  }

  it should "support nested object and no top level package" in {
    import NotDerived._

    implicitly[Cofoo[Inner.type]].shouldBe(Inner._deriving_cofoo)
  }

  it should "support nested types with dupe names" in {
    implicitly[Cofoo[nesty.Duped]].shouldBe(nesty.Duped._deriving_cofoo)
  }

}
