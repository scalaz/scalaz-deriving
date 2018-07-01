// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import org.scalatest._

import examples.Defaultz

class AltzSpec extends FlatSpec with NonImplicitAssertions {
  import Matchers._

  "anyvals" should "behave as expected" in {
    import examples.anyvals._

    Defaultz[Thing].default should equal(Thing(""))
  }

  "products" should "behave as expected" in {
    import examples.adt._

    Defaultz[Faz].default should equal(Faz(false, 0))
  }

  "coproducts" should "behave as expected" in {
    import examples.adt._

    Defaultz[Foo].default should equal(Bar(""))
  }

  "recursive products" should "behave as expected" in {
    import examples.recadt._

    Defaultz[Leaf].default should equal(Leaf(""))
  }

  "recursive coproducts" should "behave as expected" in {
    import examples.recadt._

    Defaultz[ATree].default should equal(Leaf(""))
  }

  "recursive GADT products" should "behave as expected" in {
    import examples.recgadt._

    Defaultz[GLeaf[String]].default should equal(GLeaf(""))
  }

  "recursive GADT coproducts" should "behave as expected" in {
    import examples.recgadt._

    Defaultz[GTree[String]].default should equal(GLeaf(""))
  }

}
