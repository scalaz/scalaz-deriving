// Copyright: 2017 - 2024 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import java.lang.String
import scala.{ AnyVal, Boolean, Int }

import scalaz._, Scalaz._
import scalaz.annotation.deriving
import scalaz.annotation.xderiving

package anyvals {
  @xderiving(Show)
  final case class Thing(s: String) extends AnyVal

  @deriving(Show)
  final case class Thong(s: String)        extends AnyVal
}

package adt {
  @deriving(Show)
  sealed trait Foo
  @deriving(Show)
  final case class Bar(s: String)          extends Foo
  @deriving(Show)
  final case class Faz(b: Boolean, i: Int) extends Foo
  @deriving(Show)
  final case object Baz                    extends Foo

  // the @deriving is implied by the parent!
  final case class Box(i: Int)                                 extends Foo
  final case object Flooz                                      extends Foo
}

// more complex recursive type example
package recadt {
  @deriving(Show)
  sealed trait ATree
  @deriving(Show)
  final case class Leaf(value: String)                         extends ATree
  @deriving(Show)
  final case class Branch(left: ATree, right: ATree)           extends ATree
}

// more complex recursive GADT type example
package recgadt {
  @deriving(Show)
  sealed trait GTree[A]
  @deriving(Show)
  final case class GLeaf[A](value: A)                          extends GTree[A]
  @deriving(Show)
  final case class GBranch[A](left: GTree[A], right: GTree[A]) extends GTree[A]
}
