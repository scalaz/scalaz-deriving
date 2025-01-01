// Copyright: 2017 - 2025 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import java.lang.String
import scala.{ AnyVal, Boolean, Int }

import org.scalacheck.Arbitrary
import scalaz.annotation.deriving
import scalaz.annotation.xderiving
import scalaz.scalacheck.ScalaCheckBinding._

package anyvals {
  @xderiving(Arbitrary)
  final case class Thing(s: String) extends AnyVal

  @deriving(Arbitrary)
  final case class Thong(s: String)                            extends AnyVal
}

package adt {
  @deriving(Arbitrary)
  sealed trait Foo
  final case class Bar(s: String)                              extends Foo
  final case class Faz(b: Boolean, i: Int)                     extends Foo
  final case object Baz                                        extends Foo
}

// more complex recursive type example
package recadt {
  @deriving(Arbitrary)
  sealed trait ATree
  final case class Leaf(value: String)                         extends ATree
  final case class Branch(left: ATree, right: ATree)           extends ATree
}

// more complex recursive GADT type example
package recgadt {
  @deriving(Arbitrary)
  sealed trait GTree[A]
  final case class GLeaf[A](value: A)                          extends GTree[A]
  final case class GBranch[A](left: GTree[A], right: GTree[A]) extends GTree[A]
}
