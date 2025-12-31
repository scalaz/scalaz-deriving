/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package scalaz

import examples.Default
import examples.Same
import examples.Same.ops.*
import examples.adt.*
import examples.anyvals.*
import examples.recadt.*
import examples.recgadt.*
import java.lang.String
import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.collection.immutable.List

class ExtendedInvariantAltSpec extends AnyFlatSpec with NonImplicitAssertions {
  import Matchers.*

  "Alt anyvals" should "behave as expected" in {
    Default[Thing].default should equal(Thing(""))
  }

  "Alt products" should "behave as expected" in {
    Default[Faz].default should equal(Faz(false, 0))
  }

  "Alt coproducts" should "behave as expected" in {
    Default[Foo].default should equal(Bar(""))
  }

  "Alt recursive products" should "behave as expected" in {
    Default[Leaf].default should equal(Leaf(""))
  }

  "Alt recursive coproducts" should "behave as expected" in {
    Default[ATree].default should equal(Leaf(""))
  }

  "Alt recursive GADT products" should "behave as expected" in {
    Default[GLeaf[String]].default should equal(GLeaf(""))
  }

  "Alt recursive GADT coproducts" should "behave as expected" in {
    Default[GTree[String]].default should equal(GLeaf(""))
  }

  "Alt large ADTs" should "behave as expected" in {
    import examples.bigadt.*

    Default[Bigly].default.shouldBe(BiglyO)

    Default[BiglyO.type].default.shouldBe(BiglyO)
    Default[Bigly0].default.shouldBe(Bigly0())
    Default[Bigly1].default.shouldBe(Bigly1(""))
    Default[Bigly5].default.shouldBe(Bigly5("", "", "", "", 0))
    Default[Bigly6].default.shouldBe(Bigly6(0, "", "", 0, "", ""))
    Default[Bigly7].default.shouldBe(Bigly7("", "", "", 0, 0, "", ""))
    Default[Bigly8].default.shouldBe(Bigly8(0, "", "", 0, "", "", "", 0))
  }

  val bar: Foo = Bar("hello")
  val baz: Foo = Baz
  val faz: Foo = Faz(true, 1)
  val box: Foo = Box(2)

  "Decidable anyvals" should "behave as expected" in {
    assert(Thing("greetings").same(Thing("greetings")))
    assert(Thing("greetings").different(Thing("blessings")))
  }

  "Decidable products" should "behave as expected" in {
    assert(Bar("hello").same(Bar("hello")))

    assert(Bar("hello").different(Bar("goodbye")))
  }

  "Decidable coproducts" should "behave as expected" in {
    assert(bar.same(bar))
    assert(bar.different(baz))
    assert(baz.different(bar))
    assert(baz.same(baz))
    assert(bar.different(faz))
    assert(baz.different(faz))
    assert(faz.same(faz))
    assert(box.different(bar))
    assert(box.different(baz))
    assert(box.different(faz))
    assert(box.same(box))
  }

  val leaf1: Leaf = Leaf("hello")
  val leaf2: Leaf = Leaf("goodbye")
  val branch: Branch = Branch(leaf1, leaf2)
  val tree1: ATree = Branch(leaf1, branch)
  val tree2: ATree = Branch(leaf2, branch)

  "Decidable recursive products" should "behave as expected" in {
    assert(leaf1.same(leaf1))
    assert(leaf2.same(leaf2))
    assert(leaf1.different(leaf2))
  }

  "Decidable recursive coproducts" should "behave as expected" in {
    assert(tree1.same(tree1))
    assert(tree1.different(tree2))
  }

  val gleaf1: GLeaf[String] = GLeaf("hello")
  val gleaf2: GLeaf[String] = GLeaf("goodbye")
  val gbranch: GBranch[String] = GBranch(gleaf1, gleaf2)
  val gtree1: GTree[String] = GBranch(gleaf1, gbranch)
  val gtree2: GTree[String] = GBranch(gleaf2, gbranch)

  "Decidable recursive GADT products" should "behave as expected" in {
    assert(gleaf1.same(gleaf1))
    assert(gleaf2.same(gleaf2))
    assert(gleaf1.different(gleaf2))
  }

  "Decidable recursive GADT coproducts" should "behave as expected" in {
    assert(gtree1.same(gtree1))
    assert(gtree1.different(gtree2))
  }

  "Same" should "obey the Divide composition law" in {
    val D: Divisible[Same] = Divisible[Same]

    // this could be more thorough...
    val E: Equal[Same[String]] =
      (p1, p2) => p1.same("hello", "hello") && p2.same("hello", "hello")

    val S: Same[String] = Same[String]
    assert(D.divideLaw.composition(S, S, S)(E))
  }

  "Decidable large ADTs" should "behave as expected" in {
    import examples.bigadt.*
    import scalaz.Scalaz.*

    // some product only litmus tests...
    assert(Bigly5("", "", "", "", 0).same(Bigly5("", "", "", "", 0)))
    assert(Bigly5("", "", "", "", 0).different(Bigly5("", "", "", "", 1)))

    assert(
      Bigly8(0, "", "", 0, "", "", "", 0)
        .same(Bigly8(0, "", "", 0, "", "", "", 0))
    )
    assert(
      Bigly8(0, "", "", 0, "", "", "", 0)
        .different(Bigly8(0, "", "", 0, "", "", "", 1))
    )

    // combined coproduct / product tests (fixed coproduct arity)...
    val biglys = List[Bigly](
      BiglyO,
      Bigly0(),
      Bigly1(""),
      Bigly2("", 0),
      Bigly3("", "", 0),
      Bigly4("", "", 0, ""),
      Bigly5("", "", "", "", 0),
      Bigly6(0, "", "", 0, "", ""),
      Bigly7("", "", "", 0, 0, "", ""),
      Bigly8(0, "", "", 0, "", "", "", 0)
    )

    for {
      i <- 0 |-> (biglys.length - 1)
      j <- 0 |-> (biglys.length - 1)
    } yield
      if (i == j)
        assert(biglys(i).same(biglys(j)))
      else
        assert(biglys(i).different(biglys(j)))
  }

}
