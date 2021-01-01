// Copyright: 2017 - 2021 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package examples

import java.lang.String
import scala.{ AnyVal, Boolean, Int }

import scalaz._, Scalaz._
import scalaz.annotation.deriving
import scalaz.annotation.xderiving
import org.scalacheck.Arbitrary

package anyvals {
  @xderiving(
    Equal,
    Default,
    Defaultz,
    Defaultzy,
    BadPack,
    Same,
    Show,
    Semigroup
  )
  final case class Thing(s: String) extends AnyVal

  @deriving(
    Equal,
    Default,
    Defaultz,
    Defaultzy,
    BadPack,
    Same,
    Show,
    Semigroup
  )
  final case class Thong(s: String)        extends AnyVal
}

object newtypes {
  import io.estatico.newtype.macros.newtype

  @deriving(
    Equal,
    Default,
    Defaultz,
    Defaultzy,
    BadPack,
    Same,
    Show
  )
  @newtype final case class Theng(s: String)
}

package adt {
  @deriving(
    Equal,
    Default,
    Defaultz,
    Defaultzy,
    BadPack,
    Same,
    Show
  )
  sealed trait Foo
  @deriving(
    Equal,
    Default,
    Defaultz,
    Defaultzy,
    BadPack,
    Same,
    Show,
    Monoid
  )
  final case class Bar(s: String)          extends Foo
  @deriving(
    Equal,
    Default,
    Defaultz,
    Defaultzy,
    BadPack,
    Same,
    Show
  )
  final case class Faz(b: Boolean, i: Int) extends Foo
  @deriving(
    Equal,
    Default,
    Defaultz,
    Defaultzy,
    BadPack,
    Same,
    Show,
    Semigroup
  )
  final case object Baz                    extends Foo

  // the @deriving is implied by the parent!
  final case class Box(i: Int)                                 extends Foo
  final case object Flooz                                      extends Foo
}

// more complex recursive type example
package recadt {
  @deriving(
    Arbitrary,
    Order,
    Default,
    Defaultz,
    Defaultzy,
    BadPack,
    Same,
    Show
  )
  sealed trait ATree
  @deriving(
    Arbitrary,
    Order,
    Default,
    Defaultz,
    Defaultzy,
    BadPack,
    Same,
    Show
  )
  final case class Leaf(value: String)                         extends ATree
  @deriving(
    Arbitrary,
    Order,
    Default,
    Defaultz,
    Defaultzy,
    BadPack,
    Same,
    Show
  )
  final case class Branch(left: ATree, right: ATree)           extends ATree
}

// more complex recursive GADT type example
package recgadt {
  @deriving(
    Arbitrary,
    Equal,
    Default,
    Defaultz,
    Defaultzy,
    BadPack,
    Same,
    Show
  )
  sealed trait GTree[A]
  @deriving(
    Arbitrary,
    Equal,
    Default,
    Defaultz,
    Defaultzy,
    BadPack,
    Same,
    Show
  )
  final case class GLeaf[A](value: A)                          extends GTree[A]
  @deriving(
    Arbitrary,
    Equal,
    Default,
    Defaultz,
    Defaultzy,
    BadPack,
    Same,
    Show
  )
  final case class GBranch[A](left: GTree[A], right: GTree[A]) extends GTree[A]
}

// more than 4 entries for a product and coproduct
package bigadt {

  @deriving(Same, Default)
  sealed abstract class Bigly
  @deriving(Same, Default)
  final case object BiglyO  extends Bigly
  @deriving(Same, Default)
  final case class Bigly0() extends Bigly
  @deriving(Same, Default)
  final case class Bigly1(
    a: String
  )                         extends Bigly
  @deriving(Same, Default)
  final case class Bigly2(
    a: String,
    b: Int
  )                         extends Bigly
  @deriving(Same, Default)
  final case class Bigly3(
    a: String,
    b: String,
    c: Int
  )                         extends Bigly
  @deriving(Same, Default)
  final case class Bigly4(
    a: String,
    b: String,
    c: Int,
    d: String
  )                         extends Bigly
  @deriving(Same, Default)
  final case class Bigly5(
    a: String,
    b: String,
    c: String,
    d: String,
    e: Int
  )                         extends Bigly
  @deriving(Same, Default)
  final case class Bigly6(
    a: Int,
    b: String,
    c: String,
    d: Int,
    e: String,
    f: String
  )                         extends Bigly
  @deriving(Same, Default)
  final case class Bigly7(
    a: String,
    b: String,
    c: String,
    d: Int,
    e: Int,
    f: String,
    g: String
  )                         extends Bigly
  @deriving(Same, Default)
  final case class Bigly8(
    a: Int,
    b: String,
    c: String,
    d: Int,
    e: String,
    f: String,
    g: String,
    h: Int
  )                         extends Bigly

}
