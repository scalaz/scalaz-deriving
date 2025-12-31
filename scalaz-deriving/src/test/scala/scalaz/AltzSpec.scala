/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package scalaz

import examples.Defaultz
import examples.Defaultzy
import java.lang.String
import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalaz.Scalaz.*

class AltzSpec extends AnyFlatSpec with NonImplicitAssertions {
  import Matchers.*

  "anyvals" should "behave as expected" in {
    import examples.anyvals.*

    Defaultz[Thing].default.shouldBe(Thing(""))
    Defaultzy[Thing].default.shouldBe(Thing("").just)
  }

  "products" should "behave as expected" in {
    import examples.adt.*

    Defaultz[Faz].default.shouldBe(Faz(false, 0))
    Defaultzy[Faz].default.shouldBe(Faz(false, 0).just)
  }

  "coproducts" should "behave as expected" in {
    import examples.adt.*

    Defaultz[Foo].default.shouldBe(Bar(""))
    Defaultzy[Foo].default.shouldBe(Bar("").just)
  }

  "recursive products" should "behave as expected" in {
    import examples.recadt.*

    Defaultz[Leaf].default.shouldBe(Leaf(""))
    Defaultzy[Leaf].default.shouldBe(Leaf("").just)
  }

  "recursive coproducts" should "behave as expected" in {
    import examples.recadt.*

    Defaultz[ATree].default.shouldBe(Leaf(""))
    Defaultzy[ATree].default.shouldBe(Leaf("").just)
  }

  "recursive GADT products" should "behave as expected" in {
    import examples.recgadt.*

    Defaultz[GLeaf[String]].default.shouldBe(GLeaf(""))
    Defaultzy[GLeaf[String]].default.shouldBe(GLeaf("").just)
  }

  "recursive GADT coproducts" should "behave as expected" in {
    import examples.recgadt.*

    Defaultz[GTree[String]].default.shouldBe(GLeaf(""))
    Defaultzy[GTree[String]].default.shouldBe(GLeaf("").just)
  }

}
