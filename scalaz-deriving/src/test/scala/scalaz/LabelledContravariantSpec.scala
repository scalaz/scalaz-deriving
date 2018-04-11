// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package scalaz

import java.lang.String

import org.scalatest._
import org.scalatest.Matchers._

import examples.anyvals._
import examples.adt._
import examples.recadt._
import examples.recgadt._

import Scalaz._

class LabelledContravariantSpec extends FlatSpec with NonImplicitAssertions {

  "anyvals" should "behave as expected" in {
    Thing("greetings").shows should equal("\"greetings\"")
  }

  val bar: Foo = Bar("hello")
  val baz: Foo = Baz
  val faz: Foo = Faz(true, 1)

  "products" should "behave as expected" in {
    Bar("hello").shows should equal("(s=\"hello\")")
    Baz.shows should equal("()")
    Faz(true, 1).shows should equal("(b=true,i=1)")
  }

  "coproducts" should "behave as expected" in {
    bar.shows should equal("Bar(s=\"hello\")")
    baz.shows should equal("Baz()")
    faz.shows should equal("Faz(b=true,i=1)")
  }

  val leaf1: Leaf    = Leaf("hello")
  val leaf2: Leaf    = Leaf("goodbye")
  val branch: Branch = Branch(leaf1, leaf2)
  val tree1: ATree   = Branch(leaf1, branch)
  val tree2: ATree   = Branch(leaf2, branch)

  "recursive products" should "behave as expected" in {
    leaf1.shows should equal("(value=\"hello\")")
    leaf2.shows should equal("(value=\"goodbye\")")
    branch.shows should equal(
      "(left=Leaf(value=\"hello\"),right=Leaf(value=\"goodbye\"))"
    )
  }

  "recursive coproducts" should "behave as expected" in {
    tree1.shows should equal(
      "Branch(left=Leaf(value=\"hello\"),right=Branch(left=Leaf(value=\"hello\"),right=Leaf(value=\"goodbye\")))"
    )
  }

  val gleaf1: GLeaf[String]    = GLeaf("hello")
  val gleaf2: GLeaf[String]    = GLeaf("goodbye")
  val gbranch: GBranch[String] = GBranch(gleaf1, gleaf2)
  val gtree1: GTree[String]    = GBranch(gleaf1, gbranch)
  val gtree2: GTree[String]    = GBranch(gleaf2, gbranch)

  "recursive GADT products" should "behave as expected" in {
    gleaf1.shows should equal("(value=\"hello\")")
    gleaf2.shows should equal("(value=\"goodbye\")")
    gbranch.shows should equal(
      "(left=GLeaf(value=\"hello\"),right=GLeaf(value=\"goodbye\"))"
    )
  }

  "recursive GADT coproducts" should "behave as expected" in {
    gtree1.shows should equal(
      "GBranch(left=GLeaf(value=\"hello\"),right=GBranch(left=GLeaf(value=\"hello\"),right=GLeaf(value=\"goodbye\")))"
    )
  }

}
