// Copyright: 2017 - 2018 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import java.lang.String
import scala.{ AnyVal, Boolean, Int }

import scalaz._
import Scalaz._

package anyvals {
  @xderiving(Equal, Default, Defaultz, Show)
  final case class Thing(s: String) extends AnyVal

  @deriving(Equal, Default, Defaultz, Show)
  final case class Thong(s: String) extends AnyVal
}

package adt {
  @deriving(Equal, Default, Defaultz, Show)
  sealed trait Foo
  @deriving(Equal, Default, Defaultz, Show)
  final case class Bar(s: String) extends Foo
  @deriving(Equal, Default, Defaultz, Show)
  final case class Faz(b: Boolean, i: Int) extends Foo
  @deriving(Equal, Default, Defaultz, Show)
  final case object Baz extends Foo

  // the @deriving is implied by the parent!
  final case class Box(i: Int) extends Foo
}

// more complex recursive type example
package recadt {
  @deriving(Equal, Default, Defaultz, Show)
  sealed trait ATree
  @deriving(Equal, Default, Defaultz, Show)
  final case class Leaf(value: String) extends ATree
  @deriving(Equal, Default, Defaultz, Show)
  final case class Branch(left: ATree, right: ATree) extends ATree
}

// more complex recursive GADT type example
package recgadt {
  @deriving(Equal, Default, Defaultz, Show)
  sealed trait GTree[A]
  @deriving(Equal, Default, Defaultz, Show)
  final case class GLeaf[A](value: A) extends GTree[A]
  @deriving(Equal, Default, Defaultz, Show)
  final case class GBranch[A](left: GTree[A], right: GTree[A]) extends GTree[A]
}
