// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import org.scalatest._

import examples.adt._
import examples.recadt._
import examples.recgadt._

class CovariantSpec extends FlatSpec with NonImplicitAssertions {
  import Matchers._

  "products" should "behave as expected" in {
    Default[Faz].default should equal(Faz(false, 0))
  }

  "coproducts" should "behave as expected" in {
    Default[Foo].default should equal(Bar(""))
  }

  "recursive products" should "behave as expected" in {
    Default[Leaf].default should equal(Leaf(""))
  }

  "recursive coproducts" should "behave as expected" in {
    Default[ATree].default should equal(Leaf(""))
  }

  "recursive GADT products" should "behave as expected" in {
    Default[GLeaf[String]].default should equal(GLeaf(""))
  }

  "recursive GADT coproducts" should "behave as expected" in {
    Default[GTree[String]].default should equal(GLeaf(""))
  }

}
