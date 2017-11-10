// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import org.scalatest._

//import examples.adt._
//import examples.recadt._
//import examples.recgadt._

class CovariantSpec extends FlatSpec with NonImplicitAssertions {
  import Matchers._

  "products" should "behave as expected" ignore {
    // Default[Faz].default should equal(Faz(false, 0))
    fail
  }

  "coproducts" should "behave as expected" ignore {
    // Default[Foo].default should equal(Bar(""))
    fail
  }

  "recursive products" should "behave as expected" ignore {
    // Default[Leaf].default should equal(Leaf(""))
    fail
  }

  "recursive coproducts" should "behave as expected" ignore {
    // Default[ATree].default should equal(Leaf(""))
    fail
  }

  "recursive GADT products" should "behave as expected" ignore {
    // Default[GLeaf[String]].default should equal(GLeaf(""))
    fail
  }

  "recursive GADT coproducts" should "behave as expected" ignore {
    // Default[GTree[String]].default should equal(GLeaf(""))
    fail
  }

}
