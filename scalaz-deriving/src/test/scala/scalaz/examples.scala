// Copyright: 2017 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import java.lang.String
import scala.{ Boolean, Int }

import scalaz._
import Scalaz._

package adt {
  @deriving(Equal, Default) sealed trait Foo
  @deriving(Equal, Default) final case class Bar(s: String)          extends Foo
  @deriving(Equal, Default) final case class Faz(b: Boolean, i: Int) extends Foo
  @deriving(Equal, Default) final case object Baz                    extends Foo
}

// more complex recursive type example
package recadt {
  @deriving(Equal, Default)
  sealed trait ATree
  @deriving(Equal, Default)
  final case class Leaf(value: String) extends ATree
  @deriving(Equal, Default)
  final case class Branch(left: ATree, right: ATree) extends ATree
}

// more complex recursive GADT type example
package recgadt {
  @deriving(Equal, Default)
  sealed trait GTree[A]
  @deriving(Equal, Default)
  final case class GLeaf[A](value: A) extends GTree[A]
  @deriving(Equal, Default)
  final case class GBranch[A](left: GTree[A], right: GTree[A]) extends GTree[A]
}
