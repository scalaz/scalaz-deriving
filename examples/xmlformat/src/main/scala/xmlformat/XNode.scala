/*
 * Copyright 2017 Sam Halliday
 *
 * SPDX-License-Identifier: LGPL-3.0
 */

package xmlformat

import org.scalacheck.Arbitrary
import scalaz.*
import scalaz.Scalaz.*
import scalaz.annotation.deriving
import scalaz.annotation.xderiving
import scalaz.scalacheck.ScalaCheckBinding.*
import scalaz.scalacheck.ScalazArbitrary.*

/**
 * ADT for XML, unaware of schemas, namespaces and comments (which are all
 * needlessly complicated and unnecessary for marshalling).
 */
@deriving(Equal, Show, Arbitrary)
sealed abstract class XNode

/**
 * An xml tag, also known as an element.
 *
 * `name` must be a valid https://www.w3.org/TR/xml/#NT-NameChar (not enforced).
 *
 * This is not an XNode, otherwise there is ambiguity with a one element list.
 */
@deriving(Equal, Show, Arbitrary)
final case class XTag(
  name: String,
  attrs: IList[XAttr],
  children: IList[XTag],
  body: Maybe[XString]
) {
  def asChild: XChildren = XChildren(IList.single(this))
}
object XTag {
  def apply(key: String, content: XNode): XTag =
    content match {
      case XChildren(c)   => XTag(key, IList.empty, c, Maybe.empty)
      case s @ XString(_) => XTag(key, IList.empty, IList.empty, Maybe.just(s))
    }
}

/**
 * An attribute: key and value.
 *
 * `name` must be a valid https://www.w3.org/TR/xml/#NT-AttValue (not enforced).
 */
@deriving(Show, Arbitrary)
final case class XAttr(name: String, value: XString)

object XAttr {
  implicit val equal: Equal[XAttr] =
    Equal.equalA[XAttr]
}

/**
 * A raw variant of children in an XTag.
 */
@deriving(Show)
@xderiving(Equal, Monoid, Arbitrary)
final case class XChildren(tree: IList[XTag]) extends XNode

/** String content for an XAttr value or XTag body. */
@deriving(Show)
@xderiving(Equal, Arbitrary)
final case class XString(text: String) extends XNode

object XString {
  implicit val semigroup: Semigroup[XString] =
    Semigroup.instance((a, b) => XString(s"${a.text}${b.text}"))
}
