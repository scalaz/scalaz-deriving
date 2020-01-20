// Copyright: 2017 - 2020 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import Scalaz._

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InvariantApplicativesSpec extends AnyFlatSpec with NonImplicitAssertions {
  import Matchers._

  "anyvals" should "behave as expected" in {
    import examples.anyvals._

    (Thing("hello") |+| Thing(" world")).shouldBe(Thing("hello world"))

    (Thong("hello") |+| Thong(" world")).shouldBe(Thong("hello world"))
  }

  "products" should "behave as expected" in {
    import examples.adt._

    (Bar("good") |+| Bar("bye")).shouldBe(Bar("goodbye"))

    Monoid[Bar].zero.shouldBe(Bar(""))

    (Baz |+| Baz).shouldBe(Baz)

  }

}
