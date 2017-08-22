// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalactite.tests

import scala.Predef.implicitly

import org.scalatest._
import org.scalatest.Matchers._

import play.api.libs.json

import stalactite.examples._

class StalagtiteTest extends FlatSpec {

  "@deriving" should "support case classes" in {
    implicitly[Cofoo[Foo]] shouldBe Foo.`stalactite.examples.Cofoo`
  }

  it should "support case classes with a companion" in {
    implicitly[Cofoo[Bar]] shouldBe Bar.`stalactite.examples.Cofoo`
  }

  it should "support typeclasses by FQN" in {
    fail
  }

  it should "support case classes with type parameters" in {
    fail
  }

  it should "support sealed traits" in {
    fail
  }

  it should "support baked-in rules" in {
    implicitly[json.Format[Foo]] shouldBe Foo.`play.api.libs.json.Format`
  }

  it should "support user-provided rules" in {
    implicitly[Cobar[Foo]] shouldBe Foo.`stalactite.examples.Cofoo`
  }

}
