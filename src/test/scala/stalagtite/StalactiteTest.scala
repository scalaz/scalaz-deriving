// Copyright: 2017 https://github.com/fommil/stalactite/graphs
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html
package stalactite.tests

import org.scalatest._
import org.scalatest.Matchers._
import play.api.libs.json
import shapeless.the
import stalactite.examples._

class StalactiteTest extends FlatSpec {

  "@deriving" should "support case classes" in {
    the[Cofoo[Foo]] shouldBe Foo.`stalactite.examples.Cofoo`
  }

  it should "support case classes with a companion" in {
    the[Cofoo[Bar]] shouldBe Bar.`stalactite.examples.Cofoo`
  }

  it should "support typeclasses with type parameters" in {
    // also doubles as a test of FQN handling

    the[stalactite.examples.a.Cobaz[Gaz]] should not equal null
    Gaz.`stalactite.examples.a.Cobaz` should not equal null

    the[stalactite.examples.b.Cobaz[Gaz]] should not equal null
    Gaz.`stalactite.examples.b.Cobaz` should not equal null
  }

  it should "support sealed traits" in {
    the[Cofoo[Baz]] shouldBe Baz.`stalactite.examples.Cofoo`

    the[Cobar[Baz]] shouldBe Baz.`stalactite.examples.Cobar`
  }

  it should "support baked-in rules" in {
    the[json.Format[Foo]] shouldBe Foo.`play.api.libs.json.Format`
  }

  it should "support user-provided rules" in {
    the[Cobar[Foo]] shouldBe Foo.`stalactite.examples.Cobar`
  }

}
