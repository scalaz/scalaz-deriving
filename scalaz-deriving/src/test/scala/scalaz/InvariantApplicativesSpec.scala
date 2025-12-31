/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package scalaz

import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaz.Scalaz.*

class InvariantApplicativesSpec extends AnyFlatSpec with NonImplicitAssertions {
  import Matchers.*

  "anyvals" should "behave as expected" in {
    import examples.anyvals.*

    (Thing("hello") |+| Thing(" world")).shouldBe(Thing("hello world"))

    (Thong("hello") |+| Thong(" world")).shouldBe(Thong("hello world"))
  }

  "products" should "behave as expected" in {
    import examples.adt.*

    (Bar("good") |+| Bar("bye")).shouldBe(Bar("goodbye"))

    Monoid[Bar].zero.shouldBe(Bar(""))

    (Baz |+| Baz).shouldBe(Baz)

  }

}
