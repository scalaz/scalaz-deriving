// Copyright: 2017 - 2023 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import Scalaz._

import org.scalatest._

import examples.{ Defaultz, Defaultzy }
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AltzSpec extends AnyFlatSpec with NonImplicitAssertions {
  import Matchers._

  "anyvals" should "behave as expected" in {
    import examples.anyvals._

    Defaultz[Thing].default.shouldBe(Thing(""))
    Defaultzy[Thing].default.shouldBe(Thing("").just)
  }

  "products" should "behave as expected" in {
    import examples.adt._

    Defaultz[Faz].default.shouldBe(Faz(false, 0))
    Defaultzy[Faz].default.shouldBe(Faz(false, 0).just)
  }

  "coproducts" should "behave as expected" in {
    import examples.adt._

    Defaultz[Foo].default.shouldBe(Bar(""))
    Defaultzy[Foo].default.shouldBe(Bar("").just)
  }

  "recursive products" should "behave as expected" in {
    import examples.recadt._

    Defaultz[Leaf].default.shouldBe(Leaf(""))
    Defaultzy[Leaf].default.shouldBe(Leaf("").just)
  }

  "recursive coproducts" should "behave as expected" in {
    import examples.recadt._

    Defaultz[ATree].default.shouldBe(Leaf(""))
    Defaultzy[ATree].default.shouldBe(Leaf("").just)
  }

  "recursive GADT products" should "behave as expected" in {
    import examples.recgadt._

    Defaultz[GLeaf[String]].default.shouldBe(GLeaf(""))
    Defaultzy[GLeaf[String]].default.shouldBe(GLeaf("").just)
  }

  "recursive GADT coproducts" should "behave as expected" in {
    import examples.recgadt._

    Defaultz[GTree[String]].default.shouldBe(GLeaf(""))
    Defaultzy[GTree[String]].default.shouldBe(GLeaf("").just)
  }

}
